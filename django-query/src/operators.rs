use std::fmt::Display;
use std::str::FromStr;

use crate::filtering::{FilterError, Operator, OperatorClass};

pub struct EqImpl<T> {
    target: T,
}

pub trait Equatable: core::cmp::Eq {}

impl Equatable for String {}
impl Equatable for i8 {}
impl Equatable for i16 {}
impl Equatable for i32 {}
impl Equatable for i64 {}
impl Equatable for u8 {}
impl Equatable for u16 {}
impl Equatable for u32 {}
impl Equatable for u64 {}

impl<T> Operator<T> for EqImpl<T>
where
    T: Equatable,
{
    fn apply(&self, value: &T) -> bool {
        value == &self.target
    }
}

impl<T> Operator<Option<T>> for EqImpl<T>
where
    T: Equatable,
{
    fn apply(&self, value: &Option<T>) -> bool {
        if let Some(value) = value {
            value == &self.target
        } else {
            false
        }
    }
}

pub struct Eq;

impl<T> OperatorClass<T> for Eq
where
    T: Equatable + FromStr,
    <T as FromStr>::Err: std::error::Error + Send + Sync + 'static
{
    type Instance = EqImpl<T>;
    fn instantiate(&self, rhs: &str) -> Result<Self::Instance, FilterError> {
        Ok(EqImpl {
            target: T::from_str(rhs).map_err(|e| FilterError::Instantiation(e.into()))?,
        })
    }
}

impl<T> OperatorClass<Option<T>> for Eq
where
    T: Equatable + FromStr,
    <T as FromStr>::Err: std::error::Error + Send + Sync + 'static
{
    type Instance = EqImpl<T>;
    fn instantiate(&self, rhs: &str) -> Result<Self::Instance, FilterError> {
        Ok(EqImpl {
            target: T::from_str(rhs).map_err(|e| FilterError::Instantiation(e.into()))?,
        })
    }
}

pub struct InImpl<T> {
    targets: Vec<T>,
}

impl<T> Operator<T> for InImpl<T>
where
    T: Equatable + FromStr
{
    fn apply(&self, value: &T) -> bool {
        self.targets.contains(value)
    }
}

impl<T> Operator<Option<T>> for InImpl<T>
where
    T: Equatable + FromStr
{
    fn apply(&self, value: &Option<T>) -> bool {
        if let Some(value) = value {
            self.targets.contains(value)
        } else {
            false
        }
    }
}

pub struct In;

impl<T> OperatorClass<T> for In
where
    T: Equatable + FromStr,
    <T as FromStr>::Err: std::error::Error + Send + Sync + 'static
{
    type Instance = InImpl<T>;
    fn instantiate(&self, rhs: &str) -> Result<Self::Instance, FilterError> {
        let mut targets = Vec::new();
        for elt in rhs.split(',') {
            targets.push(T::from_str(elt).map_err(|e| FilterError::Instantiation(e.into()))?);
        }
        Ok(InImpl { targets })
    }
}

impl<T> OperatorClass<Option<T>> for In
where
    T: Equatable + FromStr,
    <T as FromStr>::Err: std::error::Error + Send + Sync + 'static
{
    type Instance = InImpl<T>;
    fn instantiate(&self, rhs: &str) -> Result<Self::Instance, FilterError> {
        let mut targets = Vec::new();
        for elt in rhs.split(',') {
            targets.push(T::from_str(elt).map_err(|e| FilterError::Instantiation(e.into()))?);
        }
        Ok(InImpl { targets })
    }
}

pub struct ContainsImpl {
    target: String,
}

impl<T> Operator<T> for ContainsImpl
where
    T: Display,
{
    fn apply(&self, value: &T) -> bool {
        value.to_string().contains(&self.target)
    }
}

pub struct Contains;

impl<T> OperatorClass<T> for Contains
where
    T: Display + FromStr,
    <T as FromStr>::Err: std::error::Error + Send + Sync + 'static
{
    type Instance = ContainsImpl;
    fn instantiate(&self, rhs: &str) -> Result<Self::Instance, FilterError> {
        Ok(ContainsImpl {
            target: rhs.to_string(),
        })
    }
}

pub struct IContainsImpl {
    target: String,
}

impl<T> Operator<T> for IContainsImpl
where
    T: Display,
{
    fn apply(&self, value: &T) -> bool {
        value.to_string().to_lowercase().contains(&self.target)
    }
}

pub struct IContains;

impl<T> OperatorClass<T> for IContains
where
    T: Display + FromStr,
    <T as FromStr>::Err: std::error::Error + Send + Sync + 'static
{
    type Instance = IContainsImpl;
    fn instantiate(&self, rhs: &str) -> Result<Self::Instance, FilterError> {
        Ok(IContainsImpl {
            target: rhs.to_lowercase(),
        })
    }
}

pub struct StartsWithImpl {
    target: String,
}

impl<T> Operator<T> for StartsWithImpl
where
    T: Display,
{
    fn apply(&self, value: &T) -> bool {
        value.to_string().starts_with(&self.target)
    }
}

pub struct StartsWith;

impl<T> OperatorClass<T> for StartsWith
where
    T: Display + FromStr,
    <T as FromStr>::Err: std::error::Error + Send + Sync + 'static
{
    type Instance = StartsWithImpl;
    fn instantiate(&self, rhs: &str) -> Result<Self::Instance, FilterError> {
        Ok(StartsWithImpl {
            target: rhs.to_string(),
        })
    }
}

pub struct EndsWithImpl {
    target: String,
}

impl<T> Operator<T> for EndsWithImpl
where
    T: Display
{
    fn apply(&self, value: &T) -> bool {
        value.to_string().ends_with(&self.target)
    }
}

pub struct EndsWith;

impl<T> OperatorClass<T> for EndsWith
where
    T: Display + FromStr,
    <T as FromStr>::Err: std::error::Error + Send + Sync + 'static
{
    type Instance = EndsWithImpl;
    fn instantiate(&self, rhs: &str) -> Result<Self::Instance, FilterError> {
        Ok(EndsWithImpl {
            target: rhs.to_string(),
        })
    }
}

pub struct LessImpl<T> {
    target: T,
}

impl<T> Operator<T> for LessImpl<T>
where
    T: core::cmp::Ord,
{
    fn apply(&self, value: &T) -> bool {
        value < &self.target
    }
}

pub struct Less;

impl<T> OperatorClass<T> for Less
where
    T: Ord + FromStr,
    <T as FromStr>::Err: std::error::Error + Send + Sync + 'static
{
    type Instance = LessImpl<T>;
    fn instantiate(&self, rhs: &str) -> Result<Self::Instance, FilterError> {
        Ok(LessImpl {
            target: T::from_str(rhs).map_err(|e| FilterError::Instantiation(e.into()))?,
        })
    }
}

pub struct GreaterImpl<T> {
    target: T,
}

impl<T> Operator<T> for GreaterImpl<T>
where
    T: core::cmp::Ord,
{
    fn apply(&self, value: &T) -> bool {
        value > &self.target
    }
}

pub struct Greater;

impl<T> OperatorClass<T> for Greater
where
    T: Ord + FromStr,
    <T as FromStr>::Err: std::error::Error + Send + Sync + 'static
{
    type Instance = GreaterImpl<T>;
    fn instantiate(&self, rhs: &str) -> Result<Self::Instance, FilterError> {
        Ok(GreaterImpl {
            target: T::from_str(rhs).map_err(|e| FilterError::Instantiation(e.into()))?,
        })
    }
}

pub struct LessEqImpl<T> {
    target: T,
}

impl<T> Operator<T> for LessEqImpl<T>
where
    T: core::cmp::Ord,
{
    fn apply(&self, value: &T) -> bool {
        value <= &self.target
    }
}

pub struct LessEq;

impl<T> OperatorClass<T> for LessEq
where
    T: Ord + FromStr,
    <T as FromStr>::Err: std::error::Error + Send + Sync + 'static
{
    type Instance = LessEqImpl<T>;
    fn instantiate(&self, rhs: &str) -> Result<Self::Instance, FilterError> {
        Ok(LessEqImpl {
            target: T::from_str(rhs).map_err(|e| FilterError::Instantiation(e.into()))?,
        })
    }
}

pub struct GreaterEqImpl<T> {
    target: T,
}

impl<T> Operator<T> for GreaterEqImpl<T>
where
    T: core::cmp::Ord,
{
    fn apply(&self, value: &T) -> bool {
        value >= &self.target
    }
}

pub struct GreaterEq;

impl<T> OperatorClass<T> for GreaterEq
where
    T: Ord + FromStr,
    <T as FromStr>::Err: std::error::Error + Send + Sync + 'static
{
    type Instance = GreaterEqImpl<T>;
    fn instantiate(&self, rhs: &str) -> Result<Self::Instance, FilterError> {
        Ok(GreaterEqImpl {
            target: T::from_str(rhs).map_err(|e| FilterError::Instantiation(e.into()))?,
        })
    }
}
