use crate::environment_utils::environment::ExecutionEnvironment as EE;
use crate::environment_utils::slice_types::{TagInfo, AExpressionSlice};
use circom_algebra::algebra::ArithmeticExpression;
use compiler::hir::very_concrete_program::{Argument, TemplateInstance};
use num_bigint::BigInt;
use num_traits::ToPrimitive;
use program_structure::ast::{Expression, Access, Meta, Statement};
use program_structure::error_definition::ReportCollection;
use program_structure::program_archive::ProgramArchive;
use std::collections::HashMap;
use std::ops::{Add, AddAssign, Div, Sub};
use crate::FlagsExecution;
use program_structure::ast::ExpressionInfixOpcode;
use program_structure::ast::ExpressionPrefixOpcode;
use compiler::hir::very_concrete_program::Bounds;

type CCResult = Result<(), ReportCollection>;


pub fn compute_bounds(
    instances: &mut Vec<TemplateInstance>,
    id_to_position: &HashMap<usize, usize>,
    program_archive: &ProgramArchive,
    prime: &String
) -> CCResult {
    use program_structure::utils::constants::UsefulConstants;

    let mut reports = vec![];
    let prime = UsefulConstants::new(prime).get_p().clone();

    // First round -> compute the bounds taking into account just the statements
    for instance in instances.iter_mut() {
        let environment = transform_header_into_environment(&instance.header);
        treat_statement(&instance.code, &mut instance.signals_to_bounds, &environment, &prime);
        for (signal, bounds) in &instance.signals_to_bounds {
            println!("Signal: {}, Bounds: {:?}", signal, bounds);
        }
        println!()
    }

    // Connect the inputs with the bounds that we have computed for the father components

    // 1. For each template_instance compute the father templates that call it

    let template_to_fathers = compute_father_templates(instances);


    // EXAMPLE: Just to print the info of each template their father
    for (template_id, fathers) in &template_to_fathers{
        let pos = id_to_position[template_id];
        let template_name = instances[pos].template_name.clone();
        println!("The template {} has the following fathers: ", template_name);
        for (comp, father_id) in fathers{
            let father_pos = id_to_position[father_id];
            let father_name = &instances[father_pos].template_name;
            println!("Template {} subcomponent {}", father_name, comp);
        }
    }
    //En template_to_fathers tenemos un hashmap con clave el id de cada template y valor un vector con los nombres e ids de los templates padres
    //los bounds estan en istances, 
    // 2. Check the bounds of the signal in this father templates. Take the least precise bounds
    // Example:
    // For signal x, we look at their father. If the father component has the subcomponent com
    // check the value of the signal com.x --> update using this signals
    // Take the min for the mins and max for the maxs between all fathers

    for (template_id, fathers) in &template_to_fathers{
        //let mut new_bounds: HashMap<String, Bounds> = HashMap::new();

        let pos = id_to_position[template_id];
        let template_name = &instances[pos].template_name;
        let  templ_bounds: &mut HashMap<String, (Bounds, bool)> = &mut instances[pos].signals_to_bounds.clone();
        // Check the father templates and get the bounds
        for (comp_name, father_id) in fathers{
            let father_pos = id_to_position[father_id];
            let father_name = &instances[father_pos].template_name;
            let father_bounds = &instances[father_pos].signals_to_bounds;

            // get the bounds that are of the form
            // comp_name.signal name --> we can use the bound signal_name for the child template

            for (signal, bounds) in father_bounds{
                // split and study only if the part before the point is comp_name
                let splitted_signal_name: Vec<&str> = signal.split(".").collect();
                if splitted_signal_name[0] == comp_name{
                    // in this case it may be a input/output signal --> we need to update the child
                    if splitted_signal_name.len() > 1{
                        // in this case it is a signal of the child component --> we study it
                        // Example just to show the intuition
                        let signal_name = splitted_signal_name[1];

                        println!("The signal {} of the template {} has the following bounds in the father template {}",
                            signal_name,
                            template_name, 
                            father_name
                        );
                        println!("Bounds: {} - {}",
                            (bounds.0).min,
                            (bounds.0).max
                        );
                       let new_bounds =Bounds{
                            min: (bounds.0).min.clone().min((templ_bounds.get(signal_name).unwrap().0).min.clone()),
                            max: (bounds.0).max.clone().max((templ_bounds.get(signal_name).unwrap().0).max.clone())
                        };

                        //templ_bounds.remove_entry(signal_name);
                        templ_bounds.insert(signal_name.to_string(), (new_bounds, bounds.1));
                        
                    }   
                }
                // in other case it is not a signal of the children component, no need to study
            }
        }

    }
    

    // Second round -> compute the bounds taking into account the statements and the input values
    for instance in instances.iter_mut() {
        let environment = transform_header_into_environment(&instance.header);
        treat_statement(&instance.code, &mut instance.signals_to_bounds, &environment, &prime);
        for (signal, bounds) in &instance.signals_to_bounds {
            println!("Signal: {}, Bounds: {:?}", signal, bounds);
        }
        println!()
    }
    // To get the name of the signal do the following: 
    // subcomponent_name.in1 ==> it corresponds to the input signal in1 of subcomponent_name


    // 3. Now send the information of the outputs to all fathers
    let mut father_updates: HashMap<usize, HashMap<String, (Bounds, bool)>> = HashMap::new();
    for (template_id, fathers) in &template_to_fathers{
        //let mut new_bounds: HashMap<String, Bounds> = HashMap::new();

        let pos = id_to_position[template_id];
        let template_name = instances[pos].template_name.clone();
        let templ_bounds: HashMap<String, (Bounds, bool)> = instances[pos].signals_to_bounds.clone();//Se actualiza solo?
        // Check the father templates and get the bounds


            // get the bounds that are of the form
            // comp_name.signal name --> we can use the bound signal_name for the child template

        for (signal, bounds) in templ_bounds{
            // split and study only if the part before the point is comp_name
            
            if signal == "out"{
                for (comp_name, father_id) in fathers{
                    let father_pos = id_to_position[father_id];
                    let father_name = instances[father_pos].template_name.clone();
                    let father_bounds = father_updates
                        .get(&father_pos)
                        .cloned()
                        .unwrap_or_else(|| instances[father_pos].signals_to_bounds.clone());
                    let mut updated_father_bounds = father_bounds.clone();

                    println!("The signal {} of the template {} has the following bounds that will transfer to the father template {}",
                    signal,
                    template_name, 
                    father_name
                    );
                    println!("Bounds: [{}, {}]",
                        (bounds.0).min,
                        (bounds.0).max
                    );

                    // TODO: use this bounds for updating
                    //ANtes de insertar coger todas las de los padres.
                    //father_bounds.remove_entry(signal.to_string());
                    for (signal_father, bounds_father) in &father_bounds{
                        let splitted_signal_name: Vec<&str> = signal_father.split(".").collect();
                        if splitted_signal_name[0] == comp_name{
                            // in this case it may be a input/output signal --> we need to update the child
                            if splitted_signal_name.len() > 1{
                                let new_bounds =Bounds{
                                    min: (bounds.0).min.clone().min((bounds_father.0).min.clone()),
                                    max: (bounds.0).max.clone().max((bounds_father.0).max.clone())
                                };
                                updated_father_bounds.insert(
                                    signal_father.clone(),
                                    (new_bounds, bounds.1 && bounds_father.1),
                                );
                            }
                        }
                    }
                    father_updates.insert(father_pos, updated_father_bounds);
                    
                }
                
            }
                
                    
                  
            
            // in other case it is not a signal of the children component, no need to study
        }
    

    }

    for (father_pos, updated_father_bounds) in father_updates {
        instances[father_pos].signals_to_bounds = updated_father_bounds;
    }

    // Third round -> compute the bounds taking into account the statements the the input values, and the received out values from the fathers
    for instance in instances.iter_mut() {
        let environment = transform_header_into_environment(&instance.header);
        treat_statement(&instance.code, &mut instance.signals_to_bounds, &environment, &prime);
        for (signal, bounds) in &instance.signals_to_bounds {
            println!("Signal: {}, Bounds: {:?}", signal, bounds);
        }
        println!()
    }



    if reports.is_empty() {
        Result::Ok(())
    } else {
        Result::Err(reports)
    }
}

fn transform_header_into_environment(header: &[Argument]) -> EE {
    let mut execution_environment = EE::new();
    for arg in header {
        let name = arg.name.clone();
        let slice = argument_into_slice(arg);
        execution_environment.add_variable(&name, (TagInfo::new(), slice));
    }
    execution_environment
}

fn argument_into_slice(argument: &Argument) -> AExpressionSlice {
    use ArithmeticExpression::Number;
    let arithmetic_expressions: Vec<ArithmeticExpression<String>> =
        argument.values.iter().map(|v| Number { value: v.clone() }).collect();
    let dimensions = argument.lengths.clone();
    AExpressionSlice::new_array(dimensions, arithmetic_expressions)
}

fn treat_statement(stmt: &Statement, context: &mut HashMap<String, (Bounds, bool)>, environment: &EE, prime: &BigInt) {
    if stmt.is_initialization_block() {
        treat_init_block(stmt, context, environment, prime)
    } else if stmt.is_block() {
        treat_block(stmt, context, environment, prime)
    } else if stmt.is_if_then_else() {
        treat_conditional(stmt, context, environment, prime)
    } else if stmt.is_while() {
        treat_while(stmt, context, environment, prime)
    } else if stmt.is_substitution(){
        treat_substitution(stmt, context, environment, prime)
    } else{

    }
}

fn treat_init_block(stmt: &Statement, context: &mut HashMap<String, (Bounds, bool)>, environment: &EE, prime: &BigInt){

    use Statement::InitializationBlock;
    if let InitializationBlock { initializations, .. } = stmt {
        for init in initializations {        
            if init.is_substitution(){
                treat_statement(init, context, environment, prime);
            }
        }
    } else {
        unreachable!()
    }
}

fn treat_block(stmt: &Statement, context: &mut HashMap<String, (Bounds, bool)>, environment: &EE, prime: &BigInt) {
    use Statement::Block;
    if let Block { stmts, .. } = stmt {
        for s in stmts {
            treat_statement(s, context, environment, prime);
        }
    } else {
        unreachable!()
    }
}

fn treat_while(stmt: &Statement, context: &mut HashMap<String, (Bounds, bool)>, environment: &EE, prime: &BigInt){
    use Statement::While;
    if let While { stmt: loop_stmt, .. } = stmt {
        
        let mut temp_context: HashMap<String, (Bounds, bool)> = HashMap::new();
        
        treat_statement(loop_stmt, &mut temp_context, environment, prime);
        
        for (var, bounds) in temp_context {
            if let Some(existing) = context.get_mut(&var) {
                // Si el flag es true, usamos los bounds calculados en el loop
                if bounds.1 {
                    *existing = bounds;
                } else {
                    // Si no son bounds precisos, asignamos el rango completo y flag a false
                    *existing = (
                        Bounds { min: BigInt::from(0), max: prime.clone() - BigInt::from(1) },
                        false,
                    );
                }
            } else {
                // Si no existía en el contexto, lo insertamos tal cual
                context.insert(var, bounds);
            }
        }
    } else {
        unreachable!()  
    }
}

fn treat_conditional(stmt: &Statement, context: &mut HashMap<String, (Bounds, bool)>, environment: &EE, prime: &BigInt) {
    use Statement::IfThenElse;
    if let IfThenElse { if_case, else_case, .. } = stmt {
        let mut context_if: HashMap<String, (Bounds, bool)> = context.clone();
        let mut context_else: HashMap<String, (Bounds, bool)> = context.clone();
        treat_statement(if_case, &mut context_if, environment, prime);
        if let Some(else_case) = else_case {
            treat_statement(else_case, &mut context_else, environment, prime);
            for (var, bounds_if) in context_if {
                if let Some(bounds_else) = context_else.get(&var) {
                    let combined_bounds = Bounds {
                        min: bounds_if.0.min.min(bounds_else.0.min.clone()),
                        max: bounds_if.0.max.max(bounds_else.0.max.clone()),
                    };
                    let combined_flag = bounds_if.1 && bounds_else.1;
                    context.insert(var.clone(), (combined_bounds, combined_flag));
                } else {
                    context.insert(var.clone(), bounds_if);
                }
            }
            for (var, bounds_else) in context_else {
                if !context.contains_key(&var) {
                    context.insert(var.clone(), bounds_else.clone());
                }
            }
        }
    } else {
        unreachable!()
    }
}



fn treat_substitution(stmt: &Statement, context: &mut HashMap<String, (Bounds, bool)>, environment: &EE, prime: &BigInt) {
    use Statement::Substitution;

    fn contains_array_access(access: &Vec<Access>)-> bool{
        for acc in access{
            match acc{
                Access::ArrayAccess(_) => return true,
                Access::ComponentAccess(_) =>{}
            } 
        }
        false
    }

    if let Substitution{rhe, var, access,..} = stmt{

        // Get the complete signal name considering the component accesses
        let complete_name = treat_access_name(var, access);

        // Check if it is array or not
        let is_array_access = contains_array_access(access);


        // compute the bounds of the result and update the bounds if it is a signal
        if !is_array_access{
            context.insert(complete_name, compute_bounds_expression(rhe, context, environment, prime));
        }
        else{
            if context.contains_key(&complete_name){
                let bounds_array = context.get(&complete_name).unwrap().clone();
                let bounds_new = compute_bounds_expression(rhe, context, environment, prime);
                let combined_bounds = Bounds {
                    min: bounds_array.0.min.min(bounds_new.0.min),
                    max: bounds_array.0.max.max(bounds_new.0.max)
                };
                let combined_flag = bounds_array.1 && bounds_new.1;
                context.insert(complete_name, (combined_bounds, combined_flag));
            }
            else{
                context.insert(
                    complete_name, 
                    compute_bounds_expression(rhe, context, environment, prime)
                );
            }
            
        }

        
    } else{
        unreachable!()
    }

}

fn compute_bounds_expression(
    expr: &Expression, context: &HashMap<String, (Bounds, bool)>, environment: &EE, prime: &BigInt)
->(Bounds, bool){
    use Expression::*;
    let no_bounds = Bounds{min: BigInt::from(0), max: prime - 1};
    //println!("Computing bounds of expression");

    let (res, is_constant) = match expr{
            InfixOp{  lhe, rhe, infix_op,.. }=>compute_bounds_infix_operation(lhe, rhe, *infix_op, context, environment, prime),
            PrefixOp { rhe, prefix_op,.. }=>compute_bounds_prefix_operation(rhe, *prefix_op, context, environment, prime),
            InlineSwitchOp { if_true,if_false,.. }=>compute_bounds_in_line_switch_operation(if_true, if_false, context, environment, prime),
            ParallelOp { .. }=>(no_bounds, false),
            Variable { name, access, ..}=>get_bounds_variable(name, access, context, prime),
            Number(meta, number)=>get_number_bounds(number, prime),
            Call{ .. }=>(no_bounds, false),
            AnonymousComp{ .. }=>(no_bounds, false),
            ArrayInLine{ meta, values }=>compute_bounds_array_in_line(values, context, environment, prime),
            UniformArray{meta, value, .. }=>compute_bounds_uniform_array(value, context, environment, prime),
            Tuple {  .. }=>(no_bounds, false),
            BusCall { .. }=>(no_bounds, false),
    };
    //println!("The result is {:?}", res);
    (res, is_constant)
    
}

fn compute_bounds_infix_operation(expr_l: &Expression, expr_r: &Expression, operator: ExpressionInfixOpcode, context: &HashMap<String, (Bounds, bool)>, environment: &EE, prime: &BigInt)->(Bounds, bool){
    // check if the operands have bounds and compute the bounds of the 
    // result using them
    let bl = compute_bounds_expression(expr_l, context, environment, prime);
    let br = compute_bounds_expression(expr_r, context, environment, prime);

    

    match operator {
        program_structure::ast::ExpressionInfixOpcode::Mul =>{(
         Bounds{
            min: (bl.0.min * (br.0.min)) % prime,
            max: (bl.0.max * (br.0.max)) % prime
        }, (bl.1 && br.1)
        )},
        program_structure::ast::ExpressionInfixOpcode::Div => {(
            Bounds{
                min: BigInt::from(0),
                max: prime.clone()-1
            },false
        )},
        program_structure::ast::ExpressionInfixOpcode::Add => {(
            Bounds{
                min: (bl.0.min + br.0.min) % prime,
                max: (bl.0.max + br.0.max) % prime
            }, (bl.1 && br.1)
        )},
        program_structure::ast::ExpressionInfixOpcode::Sub => {(
            Bounds{
                min: (bl.0.min - br.0.max) % prime,
                max: (bl.0.max - br.0.min) % prime
            }, (bl.1 && br.1)
        )},
        program_structure::ast::ExpressionInfixOpcode::Pow => {(
            Bounds{
                min: bl.0.min.min(BigInt::from(1)),
                max: prime.clone()-1
            }, false
        )},
        program_structure::ast::ExpressionInfixOpcode::IntDiv => {(
            Bounds{
                min: BigInt::from(0),
                max: (bl.0.max / br.0.min) % prime
            }, (bl.1 && br.1)
        )},
        program_structure::ast::ExpressionInfixOpcode::Mod => {(
            Bounds{
                min: BigInt::from(0),//if the left operand is a multiple of the right operand, the result is 0
                max: br.0.max % prime//In Mod the result is not going to be bigger than the right operand
            }, (bl.1 && br.1)
        )},
        program_structure::ast::ExpressionInfixOpcode::ShiftL => {(
            Bounds{
                min:  (bl.0.min * 2i32.pow(br.0.min.to_u32().unwrap())) % prime,
                max:  (bl.0.max * 2i32.pow(br.0.max.to_u32().unwrap())) % prime
            }, (bl.1 && br.1)
        )},
        program_structure::ast::ExpressionInfixOpcode::ShiftR => {(
            Bounds{
                min: BigInt::from(0),
                max: bl.0.max % prime //In right shift, the result is not going to be bigger than the left operand
            }, (bl.1 && br.1)
        )},
        program_structure::ast::ExpressionInfixOpcode::LesserEq => {(
            Bounds{
                min:BigInt::from(0),// 0 or  1
                max:BigInt::from(1),// 0 or  1
            },true
        )},
        program_structure::ast::ExpressionInfixOpcode::GreaterEq => {(
            Bounds{
                min:BigInt::from(0),// 0 or  1
                max:BigInt::from(1),// 0 or  1
            },true
        )},
        program_structure::ast::ExpressionInfixOpcode::Lesser => {(
            Bounds{
                min:BigInt::from(0),// 0 or  1
                max:BigInt::from(1),// 0 or  1
            },true
        )},
        program_structure::ast::ExpressionInfixOpcode::Greater => {(
            Bounds{
                min:BigInt::from(0),// 0 or  1
                max:BigInt::from(1),// 0 or  1
            },true
        )},
        program_structure::ast::ExpressionInfixOpcode::Eq => {(
            Bounds{
                min:BigInt::from(0),// 0 or  1
                max:BigInt::from(1),// 0 or  1
            },true
        )},
        program_structure::ast::ExpressionInfixOpcode::NotEq => {(
            Bounds{
                min:BigInt::from(0),// 0 or  1
                max:BigInt::from(1),// 0 or  1
            },true
        )},
        program_structure::ast::ExpressionInfixOpcode::BoolOr =>  {(
            Bounds{
                min:BigInt::from(0),// 0 or  1
                max:BigInt::from(1),// 0 or  1
            },true
        )},
        program_structure::ast::ExpressionInfixOpcode::BoolAnd =>  {(
            Bounds{
                min:BigInt::from(0),// 0 or  1
                max:BigInt::from(1),// 0 or  1
            },true
        )},
        program_structure::ast::ExpressionInfixOpcode::BitOr => {(
            Bounds{
                min: bl.0.min.max(br.0.min) % prime,
                max: (bl.0.max + br.0.max) % prime
            }, (bl.1 && br.1)
        )},
        program_structure::ast::ExpressionInfixOpcode::BitAnd => {(
            Bounds{
                min: BigInt::from(0),
                max: bl.0.max.min(br.0.max) % prime
            }, (bl.1 && br.1)
        )},
        program_structure::ast::ExpressionInfixOpcode::BitXor => {(
            Bounds{
                min: BigInt::from(0),
                max: (bl.0.max + br.0.max) % prime
            }, (bl.1 && br.1)
        )},
    }

}

fn compute_bounds_prefix_operation(expr_r: &Expression, operator: ExpressionPrefixOpcode, context: &HashMap<String, (Bounds, bool)>, environment: &EE, prime: &BigInt)->(Bounds, bool){
        let br = compute_bounds_expression(expr_r, context, environment, prime);
        match operator{
            program_structure::ast::ExpressionPrefixOpcode::Sub => {(
                Bounds{
                    min: -br.0.max % prime,
                    max: -br.0.min % prime
                }, br.1
            )},
            program_structure::ast::ExpressionPrefixOpcode::BoolNot => {(
                Bounds{
                    min: BigInt::from(0),
                    max: BigInt::from(1)
                }, true
            )},
            program_structure::ast::ExpressionPrefixOpcode::Complement => {(
                Bounds{
                    min: BigInt::from(0),
                    max: (br.0.max * BigInt::from(2)) % prime
                }, br.1
            )},
        }
}

fn compute_bounds_in_line_switch_operation(expr_true: &Expression, expr_false: &Expression,  context: &HashMap<String, (Bounds, bool)>, environment: &EE, prime: &BigInt)->(Bounds, bool){
    let btrue = compute_bounds_expression(expr_true, context, environment, prime);
    let bfalse = compute_bounds_expression(expr_false, context, environment, prime);
    (Bounds{
        min: btrue.0.min.min(bfalse.0.min)  % prime,
        max: btrue.0.max.max(bfalse.0.max) % prime
    }, btrue.1 && bfalse.1)
}


// AUXILIAR FUNCTION TO GET THE COMPLETE SIGNAL NAME
// treat the access for the components --> study the access
fn treat_access_name(name: &String, access: &Vec<Access>) -> String{
    let mut new_name = name.clone();
    for acc in access{
        match acc{
            Access::ArrayAccess(_) =>{
                // In this case we do not need anything, all positions of the arrays 
                // have the same bounds
            }
            Access::ComponentAccess(comp_name) =>{
                // in this case we concatenate the names. We need this for the propagation
                // of input/output signal bounds
                new_name = format!("{}.{}", new_name, comp_name);
            }
        }
    }
    new_name
}

fn get_bounds_variable(name: &String, access: &Vec<Access>, context: &HashMap<String, (Bounds, bool)>, prime: &BigInt)->(Bounds, bool){
    let complete_var_name = treat_access_name(name, access);
    
    if let Some(bounds) = context.get(&complete_var_name){
        (bounds.0.clone(), true)
    } 
     else{
        (Bounds{min: BigInt::from(0), max: prime.clone()-1}, false)
    }

}
 
fn get_number_bounds(number: &BigInt, prime: &BigInt)->(Bounds, bool){
    (Bounds{min: number % prime, max: number % prime}, true)
}

fn compute_bounds_array_in_line(values: &Vec<Expression>, context: &HashMap<String, (Bounds, bool)>, environment: &EE, prime: &BigInt)->(Bounds, bool){
    let mut min = prime.clone();
    let mut max = BigInt::from(0);
    for v in values{
        let b = compute_bounds_expression(v, context, environment, prime);
        min = min.min(b.0.min) % prime;
        max = max.max(b.0.max) % prime;
    }
    (Bounds{min: min, max: max}, false)
}

fn compute_bounds_uniform_array(value: &Box<Expression>,  context: &HashMap<String, (Bounds, bool)>, environment: &EE, prime: &BigInt)->(Bounds, bool){//Para que es dimension?
    let value_bounds = compute_bounds_expression(value, context, environment, prime);
    (Bounds{
        min: value_bounds.0.min % prime,
        max: value_bounds.0.max % prime
    }, false)
}



fn compute_father_templates(
    instances: & Vec<TemplateInstance>)-> HashMap<usize, Vec<(String, usize)>>
{
    let mut template_to_fathers: HashMap<usize, Vec<(String, usize)>> = HashMap::new();
    for instance in instances{
        let father_id = instance.template_id;
        let children = &instance.triggers;
        for child in children{
            let child_id = child.template_id;
            let child_name = child.component_name.clone();

            // store that child id is the subcomponent child_name of father id

            match template_to_fathers.get_mut(&child_id){
                Some(father_list)=>{
                    father_list.push((child_name, father_id));
                }
                None => 
                {
                    template_to_fathers.insert(
                        child_id,
                        vec![(child_name, father_id)]
                    );
                }
            }
        }
    }
    template_to_fathers
}