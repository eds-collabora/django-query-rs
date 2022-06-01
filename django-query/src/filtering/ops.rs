//! Standard operators from Django for use with filtering.
//!
//! These have short names, matching their Django names, that can be used in the
//! derive macro for [Filterable](crate::filtering::Filterable). There are the following operators:
//!
//! [`Operator`]       | [`OperatorClass`] | Short name   | Restrictions
//! -------------------|-------------------|--------------|--------------
//! [`ExactImpl`]      | [`Exact`]         | `exact`      | T: [`Eq`](core::cmp::Eq)
//! [`InImpl`]         | [`In`]            | `in`         | T: [`Eq`](core::cmp::Eq)
//! [`ContainsImpl`]   | [`Contains`]      | `contains`   | T: [`Display`](std::fmt::Display)
//! [`IContainsImpl`]  | [`IContains`]     | `icontains`  | T: [`Display`](std::fmt::Display)
//! [`IExactImpl`]     | [`IExact`]        | `iexact`     | T: [`Display`](std::fmt::Display)
//! [`StartsWithImpl`] | [`StartsWith`]    | `startswith` | T: [`Display`](std::fmt::Display)
//! [`EndsWithImpl`]   | [`EndsWith`]      | `endswith`   | T: [`Display`](std::fmt::Display)
//! [`LessImpl`]       | [`Less`]          | `lt`         | T: [`Ord`](core::cmp::Ord)
//! [`GreaterImpl`]    | [`Greater`]       | `gt`         | T: [`Ord`](core::cmp::Ord)
//! [`LessEqImpl`]     | [`LessEq`]        | `lte`        | T: [`Ord`](core::cmp::Ord)
//! [`GreaterEqImpl`]  | [`GreaterEq`]     | `gte`        | T: [`Ord`](core::cmp::Ord)
//! [`IsNullImpl`]     | [`IsNull`]        | `isnull`     |

use std::fmt::Display;
use std::str::FromStr;

use crate::filtering::{FilterError, Operable, Operator, OperatorClass};

/// A value that is directly [`Operable`].
pub trait Scalar {}

impl Scalar for i8 {}
impl Scalar for u8 {}
impl Scalar for i16 {}
impl Scalar for u16 {}
impl Scalar for i32 {}
impl Scalar for u32 {}
impl Scalar for i64 {}
impl Scalar for u64 {}
impl Scalar for bool {}
impl Scalar for String {}

impl<T: chrono::TimeZone> Scalar for chrono::DateTime<T> {}

impl<T> Operable for T
where
    T: Scalar,
{
    type Base = Self;
    fn apply<O: Operator<Self::Base>>(&self, op: &O) -> bool {
        op.apply(self)
    }
}

/// Match when the value is equal to the target.
pub struct ExactImpl<T> {
    target: T,
}

impl<T> Operator<T> for ExactImpl<T>
where
    T: core::cmp::Eq,
{
    fn apply(&self, value: &T) -> bool {
        value == &self.target
    }
}

/// The [`OperatorClass`] that can instantiate [`ExactImpl`].
pub struct Exact;

impl<T> OperatorClass<T> for Exact
where
    T: core::cmp::Eq + FromStr,
    <T as FromStr>::Err: std::error::Error + Send + Sync + 'static,
{
    type Instance = ExactImpl<T>;
    fn instantiate(&self, rhs: &str) -> Result<Self::Instance, FilterError> {
        Ok(ExactImpl {
            target: T::from_str(rhs).map_err(|e| FilterError::Instantiation(e.into()))?,
        })
    }
}

/// Match when the value is equal to one of the targets.
pub struct InImpl<T> {
    targets: Vec<T>,
}

impl<T> Operator<T> for InImpl<T>
where
    T: core::cmp::Eq + FromStr,
{
    fn apply(&self, value: &T) -> bool {
        self.targets.contains(value)
    }
}

/// The [`OperatorClass`] that can instantiate [`InImpl`].
pub struct In;

impl<T> OperatorClass<T> for In
where
    T: core::cmp::Eq + FromStr,
    <T as FromStr>::Err: std::error::Error + Send + Sync + 'static,
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

/// Match when the string representation of the value contains the target.
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

/// The [`OperatorClass`] that can instantiate [`ContainsImpl`].
pub struct Contains;

impl<T> OperatorClass<T> for Contains
where
    T: Display + FromStr,
    <T as FromStr>::Err: std::error::Error + Send + Sync + 'static,
{
    type Instance = ContainsImpl;
    fn instantiate(&self, rhs: &str) -> Result<Self::Instance, FilterError> {
        Ok(ContainsImpl {
            target: rhs.to_string(),
        })
    }
}

/// Match when the string representation of the value contains the target case insensitively.
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

/// The [`OperatorClass`] that can instantiate [`IContainsImpl`].
pub struct IContains;

impl<T> OperatorClass<T> for IContains
where
    T: Display + FromStr,
    <T as FromStr>::Err: std::error::Error + Send + Sync + 'static,
{
    type Instance = IContainsImpl;
    fn instantiate(&self, rhs: &str) -> Result<Self::Instance, FilterError> {
        Ok(IContainsImpl {
            target: rhs.to_lowercase(),
        })
    }
}

/// Match when the string representation of the value is exactly the target, case insensitively.
pub struct IExactImpl {
    target: String,
}

impl<T> Operator<T> for IExactImpl
where
    T: Display,
{
    fn apply(&self, value: &T) -> bool {
        value.to_string().to_lowercase() == self.target
    }
}

/// The [`OperatorClass`] that can instantiate [`IExactImpl`].
pub struct IExact;

impl<T> OperatorClass<T> for IExact
where
    T: Display + FromStr,
    <T as FromStr>::Err: std::error::Error + Send + Sync + 'static,
{
    type Instance = IExactImpl;
    fn instantiate(&self, rhs: &str) -> Result<Self::Instance, FilterError> {
        Ok(IExactImpl {
            target: rhs.to_lowercase(),
        })
    }
}

/// Match when the string representation of the value starts with the target.
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

/// The [`OperatorClass`] that can instantiate [`StartsWithImpl`].
pub struct StartsWith;

impl<T> OperatorClass<T> for StartsWith
where
    T: Display + FromStr,
    <T as FromStr>::Err: std::error::Error + Send + Sync + 'static,
{
    type Instance = StartsWithImpl;
    fn instantiate(&self, rhs: &str) -> Result<Self::Instance, FilterError> {
        Ok(StartsWithImpl {
            target: rhs.to_string(),
        })
    }
}

/// Match when the string representation of the value ends with the target.
pub struct EndsWithImpl {
    target: String,
}

impl<T> Operator<T> for EndsWithImpl
where
    T: Display,
{
    fn apply(&self, value: &T) -> bool {
        value.to_string().ends_with(&self.target)
    }
}

/// The [`OperatorClass`] that can instantiate [`EndsWithImpl`].
pub struct EndsWith;

impl<T> OperatorClass<T> for EndsWith
where
    T: Display + FromStr,
    <T as FromStr>::Err: std::error::Error + Send + Sync + 'static,
{
    type Instance = EndsWithImpl;
    fn instantiate(&self, rhs: &str) -> Result<Self::Instance, FilterError> {
        Ok(EndsWithImpl {
            target: rhs.to_string(),
        })
    }
}

/// Match when value is less than the target.
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

/// The [`OperatorClass`] that can instantiate [`LessImpl`].
pub struct Less;

impl<T> OperatorClass<T> for Less
where
    T: Ord + FromStr,
    <T as FromStr>::Err: std::error::Error + Send + Sync + 'static,
{
    type Instance = LessImpl<T>;
    fn instantiate(&self, rhs: &str) -> Result<Self::Instance, FilterError> {
        Ok(LessImpl {
            target: T::from_str(rhs).map_err(|e| FilterError::Instantiation(e.into()))?,
        })
    }
}

/// Match when value is greater than the target.
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

/// The [`OperatorClass`] that can instantiate [`GreaterImpl`].
pub struct Greater;

impl<T> OperatorClass<T> for Greater
where
    T: Ord + FromStr,
    <T as FromStr>::Err: std::error::Error + Send + Sync + 'static,
{
    type Instance = GreaterImpl<T>;
    fn instantiate(&self, rhs: &str) -> Result<Self::Instance, FilterError> {
        Ok(GreaterImpl {
            target: T::from_str(rhs).map_err(|e| FilterError::Instantiation(e.into()))?,
        })
    }
}

/// Match when the value is less than or equal to the target.
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

/// The [`OperatorClass`] that can instantiate [`LessEqImpl`].
pub struct LessEq;

impl<T> OperatorClass<T> for LessEq
where
    T: Ord + FromStr,
    <T as FromStr>::Err: std::error::Error + Send + Sync + 'static,
{
    type Instance = LessEqImpl<T>;
    fn instantiate(&self, rhs: &str) -> Result<Self::Instance, FilterError> {
        Ok(LessEqImpl {
            target: T::from_str(rhs).map_err(|e| FilterError::Instantiation(e.into()))?,
        })
    }
}

/// Match when the value is greater than or equal to the target.
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

/// The [`OperatorClass`] that can instantiate [`GreaterEqImpl`].
pub struct GreaterEq;

impl<T> OperatorClass<T> for GreaterEq
where
    T: Ord + FromStr,
    <T as FromStr>::Err: std::error::Error + Send + Sync + 'static,
{
    type Instance = GreaterEqImpl<T>;
    fn instantiate(&self, rhs: &str) -> Result<Self::Instance, FilterError> {
        Ok(GreaterEqImpl {
            target: T::from_str(rhs).map_err(|e| FilterError::Instantiation(e.into()))?,
        })
    }
}

/// Match when there is no value, because a containing [`Option`] is [`None`].
pub struct IsNullImpl {
    target: bool,
}

impl<T> Operator<T> for IsNullImpl {
    fn apply(&self, _value: &T) -> bool {
        !self.target
    }
    fn null_option(&self) -> bool {
        self.target
    }
}

/// The [`OperatorClass`] that can instantiate [`IsNullImpl`].
pub struct IsNull;

impl<T> OperatorClass<T> for IsNull {
    type Instance = IsNullImpl;
    fn instantiate(&self, rhs: &str) -> Result<Self::Instance, FilterError> {
        Ok(IsNullImpl {
            target: bool::from_str(rhs).map_err(|e| FilterError::Instantiation(e.into()))?,
        })
    }
}
