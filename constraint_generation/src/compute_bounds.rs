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
        let template_name = &instances[pos].template_name;
        println!("The template {} has the following fathers: ", template_name);
        for (comp, father_id) in fathers{
            let father_pos = id_to_position[father_id];
            let father_name = &instances[father_pos].template_name;
            println!("Template {} subcomponent {}", father_name, comp);
        }
    }

    // 2. Check the bounds of the signal in this father templates. Take the least precise bounds
    // Example:
    // For signal x, we look at their father. If the father component has the subcomponent com
    // check the value of the signal com.x --> update using this signals
    // Take the min for the mins and max for the maxs between all fathers

    for (template_id, fathers) in &template_to_fathers{
        let new_bounds: HashMap<String, Bounds> = HashMap::new();

        let pos = id_to_position[template_id];
        let template_name = &instances[pos].template_name;
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
                            bounds.min,
                            bounds.max
                        );

                        // TODO: use this bounds for updating

                    }
                }
                // in other case it is not a signal of the children component, no need to study
            }
        }

    }
    

    // To get the name of the signal do the following: 
    // subcomponent_name.in1 ==> it corresponds to the input signal in1 of subcomponent_name


    // 3. Now send the information of the outputs to all fathers

    // 4. Recalculate again



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

fn treat_statement(stmt: &Statement, context: &mut HashMap<String, Bounds>, environment: &EE, prime: &BigInt) {
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

fn treat_init_block(stmt: &Statement, context: &mut HashMap<String, Bounds>, environment: &EE, prime: &BigInt){

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

fn treat_block(stmt: &Statement, context: &mut HashMap<String, Bounds>, environment: &EE, prime: &BigInt) {
    use Statement::Block;
    if let Block { stmts, .. } = stmt {
        for s in stmts {
            treat_statement(s, context, environment, prime);
        }
    } else {
        unreachable!()
    }
}

fn treat_while(stmt: &Statement, context: &mut HashMap<String, Bounds>, environment: &EE, prime: &BigInt){
    use Statement::While;
    if let While { stmt: loop_stmt, .. } = stmt {
        
        let mut temp_context = HashMap::new();
        
        treat_statement(loop_stmt, &mut temp_context, environment, prime);
        
        for (var, bounds) in temp_context {
            if context.contains_key(&var) {//Si no varía el rango, se puede mantener el rango
                //context.insert(var.clone(), Bounds { min: BigInt::from(0), max: prime.clone() - 1 });
                
            }

            // TODO: we need to make this more precise, in case the bounds do not depend of other signals
            context.insert(var,bounds);
        }
    } else {
        unreachable!()  
    }
}

fn treat_conditional(stmt: &Statement, context: &mut HashMap<String, Bounds>, environment: &EE, prime: &BigInt) {
    use Statement::IfThenElse;
    if let IfThenElse { if_case, else_case, .. } = stmt {
        let mut context_if: HashMap<String, Bounds> = context.clone();
        let mut context_else: HashMap<String, Bounds> = context.clone();
        treat_statement(if_case, &mut context_if, environment, prime);
        if let Some(else_case) = else_case {
            treat_statement(else_case, &mut context_else, environment, prime);
            for (var, bounds_if) in context_if {
                 if let Some(bounds_else) = context_else.get(&var) {
                    context.insert(var.clone(), Bounds{
                        min: bounds_if.min.min(bounds_else.min.clone()),
                        max: bounds_if.max.max(bounds_else.max.clone())
                    });
                }
                else{
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



fn treat_substitution(stmt: &Statement, context: &mut HashMap<String, Bounds>, environment: &EE, prime: &BigInt) {
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
                context.insert(complete_name, Bounds{
                    min: bounds_array.min.min(bounds_new.min),
                    max: bounds_array.max.max(bounds_new.max)
                });
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
    expr: &Expression, context: &HashMap<String, Bounds>, environment: &EE, prime: &BigInt)
->Bounds{
    use Expression::*;
    let no_bounds = Bounds{min: BigInt::from(0), max: prime - 1};
    //println!("Computing bounds of expression");

    let res = match expr{
            InfixOp{  lhe, rhe, infix_op,.. }=>compute_bounds_infix_operation(lhe, rhe, *infix_op, context, environment, prime),
            PrefixOp { rhe, prefix_op,.. }=>compute_bounds_prefix_operation(rhe, *prefix_op, context, environment, prime),
            InlineSwitchOp { if_true,if_false,.. }=>compute_bounds_in_line_switch_operation(if_true, if_false, context, environment, prime),
            ParallelOp { .. }=>no_bounds,
            Variable { name, access, ..}=>get_bounds_variable(name, access, context, prime),
            Number(meta, number)=>get_number_bounds(number, prime),
            Call{ .. }=>no_bounds,
            AnonymousComp{ .. }=>no_bounds,
            ArrayInLine{ meta, values }=>compute_bounds_array_in_line(values, context, environment, prime),
            UniformArray{meta, value, .. }=>compute_bounds_uniform_array(value, context, environment, prime),
            Tuple {  .. }=>no_bounds,
            BusCall { .. }=>no_bounds,
    };
    //println!("The result is {:?}", res);
    res
    
}

fn compute_bounds_infix_operation(expr_l: &Expression, expr_r: &Expression, operator: ExpressionInfixOpcode, context: &HashMap<String, Bounds>, environment: &EE, prime: &BigInt)->Bounds{
    // check if the operands have bounds and compute the bounds of the 
    // result using them
    let bl = compute_bounds_expression(expr_l, context, environment, prime);
    let br = compute_bounds_expression(expr_r, context, environment, prime);


    match operator {
        program_structure::ast::ExpressionInfixOpcode::Mul =>{
         Bounds{
            min: (bl.min * (br.min)) % prime,
            max: (bl.max * (br.max)) % prime
        }},
        program_structure::ast::ExpressionInfixOpcode::Div => Bounds{
            min: BigInt::from(0),
            max: prime.clone()-1
        },
        program_structure::ast::ExpressionInfixOpcode::Add => Bounds{
            min: (bl.min + br.min) % prime,
            max: (bl.max + br.max) % prime
        },
        program_structure::ast::ExpressionInfixOpcode::Sub => Bounds{
            min: (bl.min - br.max) % prime,
            max: (bl.max - br.min) % prime
        },
        program_structure::ast::ExpressionInfixOpcode::Pow => Bounds{
            min: bl.min.min(BigInt::from(1)),
            max: prime.clone()-1
        },
        program_structure::ast::ExpressionInfixOpcode::IntDiv => Bounds{
            min: BigInt::from(0),
            max: (bl.max /br.min) % prime
        },
        program_structure::ast::ExpressionInfixOpcode::Mod => Bounds{
            min: BigInt::from(0),//if the left operand is a multiple of the right operand, the result is 0
            max: br.max % prime//In Mod the result is not going to be bigger than the right operand
        },
        program_structure::ast::ExpressionInfixOpcode::ShiftL => Bounds{
            min:  (bl.min * 2i32.pow(br.min.to_u32().unwrap())) % prime,
            max:  (bl.max * 2i32.pow(br.max.to_u32().unwrap())) % prime
        },
        program_structure::ast::ExpressionInfixOpcode::ShiftR => Bounds{
            min: BigInt::from(0),
            max: bl.max % prime //In right shift, the result is not going to be bigger than the left operand
        },
        program_structure::ast::ExpressionInfixOpcode::LesserEq => Bounds{
            min:BigInt::from(0),// 0 or  1
            max:BigInt::from(1),// 0 or  1
        },
        program_structure::ast::ExpressionInfixOpcode::GreaterEq => Bounds{
            min: BigInt::from(0),// 0 or  1
            max: BigInt::from(1),// 0 or  1
        },
        program_structure::ast::ExpressionInfixOpcode::Lesser => Bounds{
            min: BigInt::from(0),// 0 or  1
            max: BigInt::from(1),// 0 or  1
        },
        program_structure::ast::ExpressionInfixOpcode::Greater => Bounds{
            min: BigInt::from(0),// 0 or  1
            max: BigInt::from(1),// 0 or  1
        },
        program_structure::ast::ExpressionInfixOpcode::Eq => Bounds{
            min: BigInt::from(0),// 0 or  1
            max: BigInt::from(1),// 0 or  1
        },
        program_structure::ast::ExpressionInfixOpcode::NotEq => Bounds{
            min: BigInt::from(0),// 0 or  1
            max: BigInt::from(1),// 0 or  1
        },
        program_structure::ast::ExpressionInfixOpcode::BoolOr => Bounds{
            min: BigInt::from(0),// 0 or  1
            max: BigInt::from(1),// 0 or  1
        },
        program_structure::ast::ExpressionInfixOpcode::BoolAnd => Bounds{
            min: BigInt::from(0),// 0 or  1
            max: BigInt::from(1),// 0 or  1
        },
        program_structure::ast::ExpressionInfixOpcode::BitOr => Bounds{
            min: bl.min.max(br.min),
            max: (bl.max + br.max) % prime
        },
        program_structure::ast::ExpressionInfixOpcode::BitAnd => Bounds{
            min: BigInt::from(0),
            max: bl.max.min(br.max)
        },
        program_structure::ast::ExpressionInfixOpcode::BitXor => Bounds{
           min: BigInt::from(0),
            max: (bl.max + br.max) % prime
        },
    }

}

fn compute_bounds_prefix_operation(expr_r: &Expression, operator: ExpressionPrefixOpcode, context: &HashMap<String, Bounds>, environment: &EE, prime: &BigInt)->Bounds{
        let br = compute_bounds_expression(expr_r, context, environment, prime);
        match operator{
            program_structure::ast::ExpressionPrefixOpcode::Sub => Bounds{
                min: -br.max+(prime.clone() -1),
                max: -br.min+(prime.clone() -1)
            },
            program_structure::ast::ExpressionPrefixOpcode::BoolNot => Bounds{
                min: BigInt::from(0),
                max: BigInt::from(1)
            },
            program_structure::ast::ExpressionPrefixOpcode::Complement => Bounds{
                min: BigInt::from(0),
                max: (br.max * BigInt::from(2)) % prime
            }
        }
}

fn compute_bounds_in_line_switch_operation(expr_true: &Expression, expr_false: &Expression,  context: &HashMap<String, Bounds>, environment: &EE, prime: &BigInt)->Bounds{
    let btrue = compute_bounds_expression(expr_true, context, environment, prime);
    let bfalse = compute_bounds_expression(expr_false, context, environment, prime);
    Bounds{
        min: btrue.min.min(bfalse.min),
        max: btrue.max.max(bfalse.max)
    }
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

fn get_bounds_variable(name: &String, access: &Vec<Access>, context: &HashMap<String, Bounds>, prime: &BigInt)->Bounds{
    let complete_var_name = treat_access_name(name, access);
    
    if let Some(bounds) = context.get(&complete_var_name){
        bounds.clone()
    } 
     else{
        Bounds{min: BigInt::from(0), max: prime.clone()-1}
    }

}
 
fn get_number_bounds(number: &BigInt, prime: &BigInt)->Bounds{//Funciona con los negativos? 
    Bounds{min: number % prime, max: number % prime}    
}

fn compute_bounds_array_in_line(values: &Vec<Expression>, context: &HashMap<String, Bounds>, environment: &EE, prime: &BigInt)->Bounds{
    let mut min = prime.clone();
    let mut max = BigInt::from(0);
    for v in values{
        let b = compute_bounds_expression(v, context, environment, prime);
        min = min.min(b.min);
        max = max.max(b.max);
    }
    Bounds{min: min, max: max}
}

fn compute_bounds_uniform_array(value: &Box<Expression>,  context: &HashMap<String, Bounds>, environment: &EE, prime: &BigInt)->Bounds{//Para que es dimension?
    let value_bounds = compute_bounds_expression(value, context, environment, prime);
    Bounds{
        min: value_bounds.min,
        max: value_bounds.max
    }
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