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
use crate::FlagsExecution;
use program_structure::ast::ExpressionInfixOpcode;
use compiler::hir::very_concrete_program::Bounds;

type CCResult = Result<(), ReportCollection>;


pub fn compute_bounds(
    instances: &mut Vec<TemplateInstance>,
    program_archive: &ProgramArchive,
    prime: &String
) -> CCResult {
    let mut reports = vec![];
    for instance in instances {
        let environment = transform_header_into_environment(&instance.header);
        treat_statement(&instance.code, &mut instance.signals_to_bounds, &environment, prime);
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

fn treat_statement(stmt: &Statement, context: &HashMap<String, Bounds>, environment: &EE, prime: &String) {
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

fn treat_init_block(stmt: &Statement, context: &HashMap<String, Bounds>, environment: &EE, prime: &String){

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

fn treat_block(stmt: &Statement, context: &HashMap<String, Bounds>, environment: &EE, prime: &String) {
    use Statement::Block;
    if let Block { stmts, .. } = stmt {
        for s in stmts {
            treat_statement(s, context, environment, prime);
        }
    } else {
        unreachable!()
    }
}

fn treat_while(stmt: &Statement, context: &HashMap<String, Bounds>, environment: &EE, prime: &String){
    use Statement::While;
    if let While { stmt, .. } = stmt {
        //TODO
    } else {
        unreachable!()
    }
}

fn treat_conditional(stmt: &Statement, context: &HashMap<String, Bounds>, environment: &EE, prime: &String) {
    use Statement::IfThenElse;
    if let IfThenElse { if_case, else_case, .. } = stmt {
        //TODO
    } else {
        unreachable!()
    }
}


fn treat_substitution(stmt: &Statement, context: &HashMap<String, Bounds>, environment: &EE, prime: &String) {
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
    expr: &Expression, context: &HashMap<String, Bounds>, environment: &EE, prime: &String)
->Option<Bounds>{
    use Expression::*;

    match expr{
            InfixOp{  .. }=>todo!(),
            PrefixOp {  .. }=>todo!(),
            InlineSwitchOp { .. }=>todo!(),
            ParallelOp { .. }=>None,
            Variable { .. }=>todo!(),
            Number{  .. }=>todo!(),
            Call{ .. }=>None,
            AnonymousComp{ .. }=>None,
            ArrayInLine{ .. }=>todo!(),
            UniformArray{ .. }=>todo!(),
            Tuple {  .. }=>None,
            BusCall { .. }=>None,
    }
    
}


fn compute_bounds_infix_operation(expr_l: &Expression, expr_r: &Expression, operator: ExpressionInfixOpcode){
    // check if the operands have bounds and compute the bounds of the 
    // result using them


}