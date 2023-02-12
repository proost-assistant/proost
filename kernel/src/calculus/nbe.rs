use crate::memory::term::Payload::{Abs, App, Axiom, Decl, Prod, Sort, Var};
use crate::memory::term::Term;
use crate::memory::value::{Value, AppClosure};
use crate::memory::arena::Arena;

pub fn eval_term<'arena>(values : Vec<Value<'arena>>,term : Term<'arena>, arena: &mut Arena<'arena>) -> Result<Value<'arena>,String> {
    match *term {
        Var(index,_) => match values.get::<usize>(index.into()) {
            Some(value) => Ok(*value),
            None => Err("eval: variable not found".to_owned()),
        },
        Sort(lvl) => Ok(Value::sort(lvl,arena)),

        Prod(ty, body) => {
            let ty = eval_term(values.clone(),ty,arena)?;
            let body = AppClosure::new(body,values);
            Ok(ty.prod(body,arena))
        },      

        Abs(ty, body) => {
            let ty = eval_term(values.clone(),ty,arena)?;
            let body = AppClosure::new(body,values);
            Ok(ty.abs(body,arena))
        },
        
        App(fun,arg) => {
            let fun = eval_term(values.clone(), fun, arena)?;
            let arg = eval_term(values, arg, arena)?;

            match *fun {
                crate::memory::value::Payload::Neutral(ref head, ref spine) => {
                    let mut spine = spine.clone();
                    spine.push(arg);
                    Ok(Value::neutral(head.clone(),spine, arena))
                },
                crate::memory::value::Payload::Abs(ref ty, ref body) => {
                    let mut values = body.values.clone();
                    values.push(arg);
                    eval_term(values,body.term, arena)
                },
                _ => {Err("eval: not a function".to_owned())}
            }
        },

        
        _ => todo!()
    }
}
