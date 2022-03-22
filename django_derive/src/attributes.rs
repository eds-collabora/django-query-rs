use std::collections::BTreeMap;
use syn::ext::IdentExt;

#[derive(Debug)]
pub enum DjangoOperator {
    Standard(syn::Ident),
    Custom(syn::Ident, syn::Path),
}

impl syn::parse::Parse for DjangoOperator {
    fn parse(input: syn::parse::ParseStream<'_>) -> syn::Result<Self> {
        let op = input.call(syn::Ident::parse_any)?;
        if input.lookahead1().peek(syn::Token![=]) {
            let _: syn::Token![=] = input.parse()?;
            let fun = input.call(syn::Path::parse_mod_style)?;
            Ok(DjangoOperator::Custom(op, fun))
        } else {
            Ok(DjangoOperator::Standard(op))
        }
    }
}

#[derive(Debug)]
pub enum DjangoItem {
    Rename(syn::LitStr),
    Operators(Vec<DjangoOperator>),
    DefaultOperator1(syn::Ident),
    DefaultOperator2(syn::Path),
    Traversed,
    Ignored,
    Sort(Option<syn::LitStr>),
    ForeignKey(syn::LitStr),
}

impl syn::parse::Parse for DjangoItem {
    fn parse(input: syn::parse::ParseStream<'_>) -> syn::Result<Self> {
        let attr: syn::Ident = input.parse()?;
        match attr.to_string().as_str() {
            "rename" => {
                // rename = "MyString"
                let _: syn::Token![=] = input.parse()?;
                let new_name: syn::LitStr = input.parse()?;
                Ok(DjangoItem::Rename(new_name))
            }
            "default_op" => {
                let _: syn::Token![=] = input.parse()?;
                let op = input.call(syn::Ident::parse_any)?;
                Ok(DjangoItem::DefaultOperator1(op))
            }
            "default_fun" => {
                let _: syn::Token![=] = input.parse()?;
                let fun = input.call(syn::Path::parse_mod_style)?;
                Ok(DjangoItem::DefaultOperator2(fun))
            }
            "exclude" => Ok(DjangoItem::Ignored),
            "traverse" => Ok(DjangoItem::Traversed),
            "op" => {
                // op(in = MyInOperatorClass)
                let content;
                let _: syn::token::Paren = syn::parenthesized!(content in input);
                let punc = syn::punctuated::Punctuated::<DjangoOperator, syn::Token![,]>::parse_terminated(&content)?;
                Ok(DjangoItem::Operators(punc.into_iter().collect()))
            }
            "sort" => {
                if input.lookahead1().peek(syn::token::Paren) {
                    let content;
                    let _: syn::token::Paren = syn::parenthesized!(content in input);
                    let item = content.parse()?;
                    Ok(DjangoItem::Sort(Some(item)))
                } else {
                    Ok(DjangoItem::Sort(None))
                }
            }
            "foreign_key" => {
                let _: syn::Token![=] = input.parse()?;
                let key = input.parse()?;
                Ok(DjangoItem::ForeignKey(key))
            }
            _ => Err(syn::Error::new_spanned(
                attr,
                "unsupported django attribute",
            )),
        }
    }
}

#[derive(Debug)]
pub enum DjangoFiltering {
    Included {
        default_operator: (Option<syn::Ident>, Option<syn::Path>),
        operators: BTreeMap<syn::Ident, Option<syn::Path>>,
    },
    Traversed,
    Excluded,
}

#[derive(Debug)]
pub enum DjangoCell {
    Scalar,
    ForeignRow(syn::LitStr),
    Excluded,
}

#[derive(Debug)]
pub struct DjangoMeta {
    pub name: Option<syn::LitStr>,
    pub filtering: DjangoFiltering,
    pub sort: Option<Option<syn::LitStr>>,
    pub cell: DjangoCell,
}

impl syn::parse::Parse for DjangoMeta {
    fn parse(input: syn::parse::ParseStream<'_>) -> syn::Result<Self> {
        let mut field_name = None;
        let mut operators = BTreeMap::new();
        let mut defop = (None, None);
        let mut excluded = false;
        let mut traversed = false;
        let punc =
            syn::punctuated::Punctuated::<DjangoItem, syn::Token![,]>::parse_terminated(input)?;
        let mut sort = None;
        let mut foreign_key = None;

        for item in punc {
            match item {
                DjangoItem::Rename(new_name) => {
                    field_name = Some(new_name);
                }
                DjangoItem::Operators(ops) => {
                    for op in ops {
                        match op {
                            DjangoOperator::Standard(name) => {
                                operators.insert(name, None);
                            }
                            DjangoOperator::Custom(name, fun) => {
                                operators.insert(name, Some(fun));
                            }
                        }
                    }
                }
                DjangoItem::DefaultOperator2(fun) => {
                    defop = (None, Some(fun));
                }
                DjangoItem::DefaultOperator1(op) => {
                    defop = (Some(op), None);
                }
                DjangoItem::Ignored => {
                    excluded = true;
                }
                DjangoItem::Traversed => {
                    traversed = true;
                }
                DjangoItem::Sort(key) => sort = Some(key),
                DjangoItem::ForeignKey(key) => foreign_key = Some(key),
            }
        }
        let filtering = if excluded {
            DjangoFiltering::Excluded
        } else if traversed {
            DjangoFiltering::Traversed
        } else {
            DjangoFiltering::Included {
                default_operator: defop,
                operators,
            }
        };
        let cell = if excluded {
            DjangoCell::Excluded
        } else if let Some(key) = foreign_key {
            DjangoCell::ForeignRow(key)
        } else {
            DjangoCell::Scalar
        };

        Ok(Self {
            filtering,
            sort,
            cell,
            name: field_name,
        })
    }
}
