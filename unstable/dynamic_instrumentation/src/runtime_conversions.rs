//! Conversions from runtime ([`c2rust_analysis_rt`]) to `rustc` internal types.

use c2rust_analysis_rt::mir_loc::{self, MirPlace, MirProjection};
use rustc_data_structures::fingerprint::Fingerprint;
use rustc_middle::mir::{Place, PlaceElem};
use rustc_span::def_id::DefPathHash;

use crate::util::Convert;

impl Convert<Fingerprint> for mir_loc::Fingerprint {
    fn convert(self) -> Fingerprint {
        let Self(a, b) = self;
        Fingerprint::new(a, b)
    }
}

impl Convert<mir_loc::Fingerprint> for Fingerprint {
    fn convert(self) -> mir_loc::Fingerprint {
        self.as_value().into()
    }
}

impl Convert<DefPathHash> for mir_loc::DefPathHash {
    fn convert(self) -> DefPathHash {
        DefPathHash(self.0.convert())
    }
}

impl Convert<mir_loc::DefPathHash> for DefPathHash {
    fn convert(self) -> mir_loc::DefPathHash {
        mir_loc::DefPathHash(self.0.convert())
    }
}

impl Convert<MirProjection> for PlaceElem<'_> {
    fn convert(self) -> MirProjection {
        match self {
            Self::Deref => MirProjection::Deref,
            Self::Field(field_id, _) => MirProjection::Field(field_id.into()),
            Self::Index(local) => MirProjection::Index(local.into()),
            _ => MirProjection::Unsupported,
        }
    }
}

impl Convert<MirPlace> for Place<'_> {
    fn convert(self) -> MirPlace {
        MirPlace {
            local: self.local.as_u32().into(),
            projection: self.projection.iter().map(PlaceElem::convert).collect(),
        }
    }
}
