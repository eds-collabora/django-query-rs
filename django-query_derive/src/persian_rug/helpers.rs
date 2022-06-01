use proc_macro2 as pm2;

// This really belogs in the persian_rug crate, but it can't
// be in the macro crate, so for now it's here

pub enum ConstraintItem {
    Context {
        context: syn::Ident,
        equals: syn::Token![=],
        value: syn::Ident,
    },
    Access {
        access: syn::Ident,
        paren: syn::token::Paren,
        items: syn::punctuated::Punctuated<syn::Type, syn::Token![,]>,
    },
}

impl syn::parse::Parse for ConstraintItem {
    fn parse(input: syn::parse::ParseStream<'_>) -> syn::Result<Self> {
        let attr: syn::Ident = input.parse()?;
        match attr.to_string().as_str() {
            "context" => {
                let equals = input.parse()?;
                let value = input.parse()?;
                Ok(ConstraintItem::Context {
                    context: attr,
                    equals,
                    value,
                })
            }
            "access" => {
                let content;
                let paren = syn::parenthesized!(content in input);
                let items =
                    syn::punctuated::Punctuated::<syn::Type, syn::Token![,]>::parse_terminated(
                        &content,
                    )?;
                Ok(ConstraintItem::Access {
                    access: attr,
                    paren,
                    items,
                })
            }
            _ => Err(syn::Error::new_spanned(
                attr,
                "unsupported persian-rug constraint",
            )),
        }
    }
}

impl quote::ToTokens for ConstraintItem {
    fn to_tokens(&self, tokens: &mut pm2::TokenStream) {
        match self {
            Self::Context {
                context,
                equals,
                value,
            } => {
                tokens.extend(quote::quote! {
                    #context #equals #value
                });
            }
            Self::Access {
                access,
                paren,
                items,
            } => {
                access.to_tokens(tokens);
                paren.surround(tokens, |tokens| items.to_tokens(tokens))
            }
        }
    }
}

// End of portion that doesn't really belong in this crate

// This doesn't really belong so deep in the tree
/// Top level attributes for the django derives
pub enum DjangoTypeAttr {
    PersianRugConstraints {
        persian_rug: syn::Ident,
        paren: syn::token::Paren,
        constraints: syn::punctuated::Punctuated<ConstraintItem, syn::Token![,]>,
    },
}

impl syn::parse::Parse for DjangoTypeAttr {
    fn parse(input: syn::parse::ParseStream<'_>) -> syn::Result<Self> {
        let attr: syn::Ident = input.parse()?;
        match attr.to_string().as_str() {
            "persian_rug" => {
                let content;
                let paren: syn::token::Paren = syn::parenthesized!(content in input);
                let punc = syn::punctuated::Punctuated::<ConstraintItem, syn::Token![,]>::parse_terminated(&content)?;
                Ok(DjangoTypeAttr::PersianRugConstraints {
                    persian_rug: attr,
                    paren,
                    constraints: punc,
                })
            }
            _ => Err(syn::Error::new_spanned(
                attr,
                "unsupported django attribute",
            )),
        }
    }
}

pub fn get_persian_rug_constraints(
    attrs: &Vec<syn::Attribute>,
) -> syn::Result<(syn::Ident, Vec<syn::Type>)> {
    let mut context = None;
    // let mut accessor = None;
    let mut used_types = Vec::new();

    for attr in attrs {
        if attr.path.is_ident("django") {
            let items = attr
                .parse_args_with(
                    syn::punctuated::Punctuated::<DjangoTypeAttr, syn::Token![,]>::parse_terminated,
                )
                .map_err(|e| {
                    syn::Error::new(e.span(), format!("failed to parse django attribute: {}", e))
                })?;

            for item in items.iter() {
                match item {
                    DjangoTypeAttr::PersianRugConstraints { constraints, .. } => {
                        for constraint in constraints {
                            match constraint {
                                ConstraintItem::Context { value, .. } => {
                                    context = Some(value.clone());
                                }
                                ConstraintItem::Access { items, .. } => {
                                    used_types.extend(items.iter().cloned());
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    let context = context.ok_or_else(|| {
        syn::Error::new(
            pm2::Span::call_site(),
            "no persian-rug context annotation provided.",
        )
    })?;
    Ok((context, used_types))
}
