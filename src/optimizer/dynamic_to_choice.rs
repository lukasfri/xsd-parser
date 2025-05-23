use crate::types::{
    ComplexInfo, ElementInfo, ElementMode, GroupInfo, Ident, Type, TypeVariant, Types, VecHelper,
};

use super::TypeTransformer;

/// This will use a enum that contains all known variants of the dynamic
/// type instead of a dynamic box.
///
/// # Examples
///
/// Consider the following XML schema.
/// ```xml
#[doc = include_str!("../../tests/optimizer/abstract.xsd")]
/// ```
///
/// Without this optimization this will result in the following code:
/// ```rust
#[doc = include_str!("../../tests/optimizer/expected0/convert_dynamic_to_choice.rs")]
/// ```
///
/// With this optimization the following code is generated:
/// ```rust
#[doc = include_str!("../../tests/optimizer/expected1/convert_dynamic_to_choice.rs")]
/// ```
#[derive(Debug)]
pub struct ConvertDynamicToChoice;

impl TypeTransformer for ConvertDynamicToChoice {
    type Error = super::Error;

    fn transform(self, types: &mut Types) -> Result<(), super::Error> {
        use std::collections::btree_map::Entry;

        tracing::debug!("convert_dynamic_to_choice");

        let idents = types
            .iter()
            .filter_map(|(ident, ty)| {
                if matches!(&ty.variant, TypeVariant::Dynamic(_)) {
                    Some(ident)
                } else {
                    None
                }
            })
            .cloned()
            .collect::<Vec<_>>();

        for ident in idents {
            let content_name = types.name_builder().shared_name("Content").finish();
            let content_ident = Ident::new(content_name).with_ns(ident.ns);

            let type_ = types.get_mut(&ident).unwrap();
            let TypeVariant::Dynamic(x) = &mut type_.variant else {
                crate::unreachable!();
            };

            let mut si = GroupInfo::default();
            for derived in &x.derived_types {
                si.elements.find_or_insert(derived.clone(), |ident| {
                    ElementInfo::new(ident, derived.clone(), ElementMode::Element)
                });
            }

            type_.variant = TypeVariant::ComplexType(ComplexInfo {
                content: Some(content_ident.clone()),
                is_dynamic: true,
                ..Default::default()
            });

            match types.entry(content_ident) {
                Entry::Vacant(e) => {
                    e.insert(Type::new(TypeVariant::Choice(si)));
                }
                Entry::Occupied(_) => crate::unreachable!(),
            }
        }

        Ok(())
    }
}
