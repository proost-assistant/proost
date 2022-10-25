use derive_more::{Display};

#[derive(Clone, Debug, Default, Display, PartialEq, Eq)]
pub enum UniverseLevel {
    #[default]
    UnivZero,
    UnivSucc(Box<UniverseLevel>),
    #[display(fmt = "max {} {}", _0, _1)]
    UnivMax(Box<UniverseLevel>,Box<UniverseLevel>),
    UnivVar(usize)
}
use UniverseLevel::*;

impl UniverseLevel {
    fn normalize(self) -> Self {
        match self {
            UnivMax(box UnivSucc(u1),box UnivSucc(u2)) => UnivSucc(box UnivMax(u1,u2)),
            UnivMax(box u1,box UnivZero) | UnivMax(box UnivZero, box u1) => u1,
            UnivMax(u1, u2) if u1.clone().normalize() == u2.clone().normalize() => u1.clone().normalize(),
            _ => self
            }
        }
    
    fn shift(self, n : usize) -> Self{
        match self {
            UnivZero => self,
            UnivSucc(u) => UnivSucc(box u.shift(n)),
            UnivMax(u1,u2) => UnivMax(box u1.shift(n), box u2.shift(n)),
            UnivVar(k) if k>n => UnivVar(k-1),
            _ => self
        }
    }

    fn substitute(self,n : usize) -> Self {
        
    }

    fn geq(self, u2 : UniverseLevel,n : u64) -> bool {
        match (self,u2) {
            (UnivZero,_) if n >=0  => true,
            (_,_) if self == u2 && n>=0 => true,
            (UnivSucc(l),_) if l.geq(u2,n-1) => true,
            (_,UnivSucc(box l)) if self.geq(l,n+1) => true,
            (_,UnivMax(box l1, box l2)) if self.geq(l1,n) || self.geq(l2,n) => true,
            (UnivMax(box l1, box l2),_) if l1.geq(u2,n) && l2.geq(u2,n) => true


        }
    }


    pub fn is_eq(self,u2 : UniverseLevel) -> bool {
        self.geq(u2,0) && u2.geq(self,0)
    }

}
