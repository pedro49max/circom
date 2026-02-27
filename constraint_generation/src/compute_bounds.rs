use crate::environment_utils::environment::ExecutionEnvironment as EE;
use crate::environment_utils::slice_types::{TagInfo, AExpressionSlice};
use circom_algebra::algebra::ArithmeticExpression;
use compiler::hir::very_concrete_program::{Argument, TemplateInstance};
use num_bigint::BigInt;
use num_traits::ToPrimitive;
use program_structure::ast::{Expression, Meta, Statement};
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
    program_archive: &ProgramArchive,
    prime: &String
) -> CCResult {
    use program_structure::utils::constants::UsefulConstants;

    let mut reports = vec![];
    let prime = UsefulConstants::new(prime).get_p().clone();

    for instance in instances {
        let environment = transform_header_into_environment(&instance.header);
        treat_statement(&instance.code, &mut instance.signals_to_bounds, &environment, &prime);
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
    if let While { stmt, .. } = stmt {
        //TODO
    } else {
        unreachable!()
    }
}

fn treat_conditional(stmt: &Statement, context: &mut HashMap<String, Bounds>, environment: &EE, prime: &BigInt) {
    use Statement::IfThenElse;
    if let IfThenElse { if_case, else_case, .. } = stmt {
        //TODO
    } else {
        unreachable!()
    }
}


fn treat_substitution(stmt: &Statement, context: &mut HashMap<String, Bounds>, environment: &EE, prime: &BigInt) {
    use Statement::Substitution;

    if let Substitution{rhe, var, ..} = stmt{
        // TODO
        // compute the bounds of the result and update the bounds if it is a signal
        
        let bounds = compute_bounds_expression(rhe, context, environment, prime);
        
    } else{
        unreachable!()
    }

}

fn compute_bounds_expression(
    expr: &Expression, context: &HashMap<String, Bounds>, environment: &EE, prime: &BigInt)
->Bounds{
    use Expression::*;
    let no_bounds = Bounds{min: BigInt::from(0), max: prime - 1};
    println!("Computing bounds of expression");

    let res = match expr{
            InfixOp{  lhe, rhe, infix_op,.. }=>compute_bounds_infix_operation(lhe, rhe, *infix_op, context, environment, prime),
            PrefixOp { rhe, prefix_op,.. }=>compute_bounds_prefix_operation(rhe, *prefix_op, context, environment, prime),
            InlineSwitchOp { if_true,if_false,.. }=>compute_bounds_in_line_switch_operation(if_true, if_false, context, environment, prime),
            ParallelOp { .. }=>no_bounds,
            Variable { name, ..}=>todo!(),
            Number(meta, number)=>todo!(),
            Call{ .. }=>no_bounds,
            AnonymousComp{ .. }=>no_bounds,
            ArrayInLine{ .. }=>todo!(),
            UniformArray{ .. }=>todo!(),
            Tuple {  .. }=>no_bounds,
            BusCall { .. }=>no_bounds,
    };
    println!("The result is {:?}", res);
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
            min: bl.min * (br.min),
            max: bl.max * (br.max)
        }},
        program_structure::ast::ExpressionInfixOpcode::Div => Bounds{
            min: BigInt::from(0),
            max: prime.clone()
        },
        program_structure::ast::ExpressionInfixOpcode::Add => Bounds{
            min: bl.min + br.min,
            max: bl.max + br.max
        },
        program_structure::ast::ExpressionInfixOpcode::Sub => Bounds{
            min: bl.min - br.max,
            max: bl.max - br.min
        },
        program_structure::ast::ExpressionInfixOpcode::Pow => Bounds{
            min: bl.min.min(BigInt::from(1)),//Either 0 or 1
            max: prime.clone()
        },
        program_structure::ast::ExpressionInfixOpcode::IntDiv => Bounds{
            min: BigInt::from(0),//if br > bl, the result is 0
            max: bl.max //In integer division, the result is not going to be biger than the dividend
        },
        program_structure::ast::ExpressionInfixOpcode::Mod => Bounds{
            min: BigInt::from(0),//if the left operand is a multiple of the right operand, the result is 0
            max: br.max//In Mod the resuult is not going to be bigger than the right operand
        },
        program_structure::ast::ExpressionInfixOpcode::ShiftL => Bounds{
            min:  bl.min * 2i32.pow(br.min.to_u32().unwrap()),
            max:  bl.max * 2i32.pow(br.max.to_u32().unwrap())
        },
        program_structure::ast::ExpressionInfixOpcode::ShiftR => Bounds{
            min: BigInt::from(0),
            max: bl.max //In right shift, the result is not going to be bigger than the left operand
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
            max: bl.max + br.max
        },
        program_structure::ast::ExpressionInfixOpcode::BitAnd => Bounds{
            min: BigInt::from(0),
            max: bl.max.min(br.max)
        },
        program_structure::ast::ExpressionInfixOpcode::BitXor => Bounds{
           min: BigInt::from(0),
            max: bl.max + br.max
        },
    }

}

fn compute_bounds_prefix_operation(expr_r: &Expression, operator: ExpressionPrefixOpcode, context: &HashMap<String, Bounds>, environment: &EE, prime: &BigInt)->Bounds{
        let br = compute_bounds_expression(expr_r, context, environment, prime);
        match operator{
            program_structure::ast::ExpressionPrefixOpcode::Sub => Bounds{
                min: -br.max,
                max: -br.min
            },
            program_structure::ast::ExpressionPrefixOpcode::BoolNot => Bounds{
                min: BigInt::from(0),
                max: BigInt::from(1)
            },
            program_structure::ast::ExpressionPrefixOpcode::Complement => Bounds{ //Cambio de pos a neg?
                min: BigInt::from(0),
                max: br.max*BigInt::from(2)
            }
        }
}

fn compute_bounds_in_line_switch_operation(expr_true: &Expression, expr_false: &Expression,  context: &HashMap<String, Bounds>, environment: &EE, prime: &BigInt)->Bounds{
    let btrue = compute_bounds_expression(expr_true, context, environment, prime);
    let bfalse = compute_bounds_expression(expr_false, context, environment, prime);
    Bounds{
        min: btrue.min.min(bfalse.min),//No se si es corecto mi entendimiento de esta operacion
        max: btrue.max.max(bfalse.max)
    }
}