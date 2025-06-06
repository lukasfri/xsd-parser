pub type Bmecat = BmecatElementType;
#[derive(Debug)]
pub struct BmecatElementType {
    pub version: TypeBmEcatVersionType,
    pub content: Vec<BmecatElementTypeContent>,
}
#[derive(Debug)]
pub enum BmecatElementTypeContent {
    Header(HeaderElementType),
    TnewCatalog(TnewCatalogElementType),
    TupdateProducts(TupdateProductsElementType),
    TupdatePrices(TupdatePricesElementType),
    TnewProductdata(TnewProductdataElementType),
}
#[derive(Debug)]
pub enum TypeBmEcatVersionType {
    _2005,
}
#[derive(Debug)]
pub struct HeaderElementType {
    pub generator_info: Option<String>,
    pub catalog: CatalogElementType,
    pub buyer: BuyerElementType,
    pub supplier: SupplierElementType,
    pub user_defined_extensions: UdxHeaderType,
}
#[derive(Debug)]
pub struct TnewCatalogElementType {
    pub product: Vec<TnewCatalogProductElementType>,
}
#[derive(Debug)]
pub struct TupdateProductsElementType {
    pub prev_version: i32,
    pub product: Vec<TupdateProductsProductElementType>,
}
#[derive(Debug)]
pub struct TupdatePricesElementType {
    pub prev_version: i32,
    pub product: Vec<TupdatePricesProductElementType>,
}
#[derive(Debug)]
pub struct TnewProductdataElementType {
    pub product: Vec<TnewProductdataProductElementType>,
}
#[derive(Debug)]
pub struct CatalogElementType {
    pub language: Vec<LanguageElementType>,
    pub catalog_id: String,
    pub catalog_version: String,
    pub catalog_name: Vec<DtMlstringType>,
    pub datetime: CatalogDatetimeElementType,
    pub territory: Vec<String>,
    pub currency: Option<DtCurrenciesType>,
    pub mime_root: Vec<DtMlstringType>,
}
#[derive(Debug)]
pub struct BuyerElementType {
    pub buyer_id: Vec<TypePartyIdType>,
    pub buyer_name: String,
}
#[derive(Debug)]
pub struct SupplierElementType {
    pub supplier_id: Vec<TypePartyIdType>,
    pub supplier_name: String,
    pub address: Option<SupplierAddressElementType>,
    pub mime_info: Option<MimeInfoElementType>,
}
#[derive(Debug)]
pub struct UdxHeaderType {
    pub udx_edxf_version: TypeBmEcatEtimVersionType,
}
#[derive(Debug)]
pub struct TnewCatalogProductElementType {
    pub mode: TnewCatalogProductmodeType,
    pub supplier_pid: String,
    pub product_details: ProductDetailsElementType,
    pub product_features: Vec<ProductFeaturesElementType>,
    pub product_order_details: ProductOrderDetailsElementType,
    pub product_price_details: Vec<ProductPriceDetailsElementType>,
    pub user_defined_extensions: UdxProductType,
    pub product_reference: Vec<ProductReferenceElementType>,
    pub product_logistic_details: Option<ProductLogisticDetailsElementType>,
}
#[derive(Debug)]
pub struct TupdateProductsProductElementType {
    pub mode: TupdateProductsProductmodeType,
    pub supplier_pid: String,
    pub product_details: ProductDetailsElementType,
    pub product_features: Vec<ProductFeaturesElementType>,
    pub product_order_details: ProductOrderDetailsElementType,
    pub product_price_details: Vec<ProductPriceDetailsElementType>,
    pub user_defined_extensions: Option<UdxProductType>,
    pub product_reference: Vec<ProductReferenceElementType>,
    pub product_logistic_details: Option<ProductLogisticDetailsElementType>,
}
#[derive(Debug)]
pub struct TupdatePricesProductElementType {
    pub mode: TupdatePricesProductmodeType,
    pub supplier_pid: String,
    pub product_price_details: Vec<ProductPriceDetailsElementType>,
    pub user_defined_extensions: Option<UdxProductType>,
}
#[derive(Debug)]
pub struct TnewProductdataProductElementType {
    pub mode: TnewProductdataProductmodeType,
    pub supplier_pid: String,
    pub product_details: ProductDetailsElementType,
    pub product_features: Vec<ProductFeaturesElementType>,
    pub user_defined_extensions: Option<UdxProductdataType>,
    pub product_reference: Vec<ProductReferenceElementType>,
}
#[derive(Debug)]
pub struct LanguageElementType {
    pub default: Option<String>,
    pub content: DtLangType,
}
#[derive(Debug)]
pub struct DtMlstringType {
    pub lang: Option<DtLangType>,
    pub content: String,
}
#[derive(Debug)]
pub struct CatalogDatetimeElementType {
    pub type_: CatalogDatetimetypeType,
    pub date: String,
}
#[derive(Debug)]
pub enum DtCurrenciesType {
    Adp,
    Aed,
    Afa,
    Afn,
    All,
    Amd,
    Ang,
    Aoa,
    Aok,
    Ara,
    Ars,
    Ats,
    Aud,
    Awg,
    Azn,
    Bam,
    Bbd,
    Bdt,
    Bef,
    Bgl,
    Bgn,
    Bhd,
    Bif,
    Bmd,
    Bnd,
    Bob,
    Bov,
    Brc,
    Brl,
    Bsd,
    Btn,
    Buk,
    Bwp,
    Byn,
    Bzd,
    Cad,
    Cdf,
    Che,
    Chf,
    Chw,
    Clf,
    Clp,
    Cny,
    Cop,
    Cou,
    Crc,
    Csk,
    Cuc,
    Cup,
    Cve,
    Cyp,
    Czk,
    Ddm,
    Dem,
    Djf,
    Dkk,
    Dop,
    Dzd,
    Ecs,
    Egp,
    Ern,
    Esp,
    Etb,
    Eur,
    Fim,
    Fjd,
    Fkp,
    Frf,
    Gbp,
    Gel,
    Ghc,
    Ghs,
    Gip,
    Gmd,
    Gnf,
    Grd,
    Gtq,
    Gwp,
    Gyd,
    Hkd,
    Hnl,
    Hrk,
    Htg,
    Huf,
    Idr,
    Iep,
    Ils,
    Inr,
    Iqd,
    Irr,
    Isk,
    Itl,
    Jmd,
    Jod,
    Jpy,
    Kes,
    Kgs,
    Khr,
    Kmf,
    Kpw,
    Krw,
    Kwd,
    Kyd,
    Kzt,
    Lak,
    Lbp,
    Lkr,
    Lrd,
    Lsl,
    Luf,
    Lyd,
    Mad,
    Mdl,
    Mga,
    Mgf,
    Mkd,
    Mmk,
    Mnt,
    Mop,
    Mro,
    Mru,
    Mtl,
    Mur,
    Mvr,
    Mwk,
    Mxn,
    Mxp,
    Mxv,
    Myr,
    Mzm,
    Mzn,
    Nad,
    Ngn,
    Nic,
    Nio,
    Nlg,
    Nok,
    Npr,
    Nzd,
    Omr,
    Pab,
    Pei,
    Pen,
    Pgk,
    Php,
    Pkr,
    Plz,
    Pln,
    Pte,
    Pyg,
    Qar,
    Rol,
    Ron,
    Rsd,
    Rub,
    Rwf,
    Sar,
    Sbd,
    Scr,
    Sdg,
    Sdp,
    Sek,
    Sgd,
    Shp,
    Sll,
    Skk,
    Sos,
    Srd,
    Srg,
    Ssp,
    Std,
    Stn,
    Sur,
    Svc,
    Syp,
    Szl,
    Thb,
    Tjs,
    Tmt,
    Tnd,
    Top,
    Tpe,
    Trl,
    Try,
    Ttd,
    Twd,
    Tzs,
    Uah,
    Ugs,
    Ugx,
    Usd,
    Usn,
    Uyi,
    Uyp,
    Uyu,
    Uyw,
    Uzs,
    Veb,
    Ves,
    Vnd,
    Vuv,
    Wst,
    Xaf,
    Xag,
    Xau,
    Xba,
    Xbb,
    Xbc,
    Xbd,
    Xcd,
    Xdr,
    Xof,
    Xpd,
    Xpf,
    Xpt,
    Xsu,
    Xts,
    Xua,
    Xxx,
    Ydd,
    Yer,
    Yud,
    Zar,
    Zmk,
    Zmw,
    Zrz,
    Zwd,
    Zwl,
}
#[derive(Debug)]
pub struct TypePartyIdType {
    pub type_: String,
    pub content: String,
}
#[derive(Debug)]
pub struct SupplierAddressElementType {
    pub type_: SupplierAddresstypeType,
    pub contact: Vec<DtMlstringType>,
    pub street: Vec<DtMlstringType>,
    pub zip: Vec<DtMlstringType>,
    pub city: Vec<DtMlstringType>,
    pub country: Vec<DtMlstringType>,
    pub vat_id: Option<String>,
    pub email: String,
    pub url: Option<String>,
}
#[derive(Debug)]
pub struct MimeInfoElementType {
    pub mime: Vec<MimeElementType>,
}
#[derive(Debug)]
pub enum TypeBmEcatEtimVersionType {
    _50,
}
#[derive(Debug)]
pub enum TnewCatalogProductmodeType {
    New,
}
#[derive(Debug)]
pub struct ProductDetailsElementType {
    pub description_short: Vec<DtMlstringType>,
    pub description_long: Vec<DtMlstringType>,
    pub international_pid: Vec<InternationalPidElementType>,
    pub supplier_alt_pid: Option<String>,
    pub buyer_pid: Option<BuyerPidElementType>,
    pub manufacturer_pid: Option<String>,
    pub manufacturer_name: Option<String>,
    pub manufacturer_type_descr: Vec<DtMlstringType>,
    pub delivery_time: Option<f64>,
    pub special_treatment_class: Vec<SpecialTreatmentClassElementType>,
    pub keyword: Vec<DtMlstringType>,
    pub remarks: Vec<DtMlstringType>,
    pub product_status: Vec<ProductStatusElementType>,
    pub product_type: Option<ProductTypeElementType>,
}
#[derive(Debug)]
pub struct ProductFeaturesElementType {
    pub reference_feature_system_name: TypeClassificationSystemNameType,
    pub reference_feature_group_id: String,
    pub feature: Vec<FeatureElementType>,
}
#[derive(Debug)]
pub struct ProductOrderDetailsElementType {
    pub order_unit: DtUnitType,
    pub content_unit: DtUnitType,
    pub no_cu_per_ou: Option<f64>,
    pub price_quantity: Option<f64>,
    pub quantity_min: Option<f32>,
    pub quantity_interval: Option<f32>,
}
#[derive(Debug)]
pub struct ProductPriceDetailsElementType {
    pub datetime: Vec<ProductPriceDetailsDatetimeElementType>,
    pub daily_price: Option<String>,
    pub product_price: Vec<ProductPriceElementType>,
    pub price_base: Option<PriceBaseElementType>,
}
#[derive(Debug)]
pub struct UdxProductType {
    pub udx_edxf_mime_info: Option<UdxEdxfMimeInfoElementType>,
    pub udx_edxf_manufacturer_acronym: Option<String>,
    pub udx_edxf_description_very_short: Vec<DtMlstringType>,
    pub udx_edxf_brand_name: Option<String>,
    pub udx_edxf_tender_text: Vec<DtMlstringType>,
    pub udx_edxf_valid_from: Option<String>,
    pub udx_edxf_expiration_date: Option<String>,
    pub udx_edxf_discount_group: Option<UdxEdxfDiscountGroupElementType>,
    pub udx_edxf_bonus_group_supplier: Option<String>,
    pub udx_edxf_additional_factors: Option<UdxEdxfAdditionalFactorsElementType>,
    pub udx_edxf_product_to_stock: Option<String>,
    pub udx_edxf_product_series: Vec<DtMlstringType>,
    pub udx_edxf_product_variation: Vec<DtMlstringType>,
    pub udx_edxf_predecessor_pid: Vec<String>,
    pub udx_edxf_country_branch_numbers: Option<UdxEdxfCountryBranchNumbersElementType>,
    pub udx_edxf_country_branch_supplier_ids: Option<UdxEdxfCountryBranchSupplierIdsElementType>,
    pub udx_edxf_packing_units: Option<UdxEdxfPackingUnitsElementType>,
    pub udx_edxf_product_logistic_details: Option<UdxEdxfProductLogisticDetailsElementType>,
    pub udx_edxf_shelf_life_period: Option<i32>,
    pub udx_edxf_battery_contained: Option<String>,
    pub udx_edxf_rohs_indicator: Option<UdxEdxfRohsIndicatorElementType>,
    pub udx_edxf_ce_marking: Option<String>,
    pub udx_edxf_reach: Option<UdxEdxfReachElementType>,
    pub udx_edxf_special_treatment_class_details:
        Option<UdxEdxfSpecialTreatmentClassDetailsElementType>,
    pub udx_edxf_surcharge_list: Option<UdxEdxfSurchargeListElementType>,
    pub udx_edxf_warranty: Option<UdxEdxfWarrantyElementType>,
    pub udx_edxf_product_etim_dynamic: Option<UdxEdxfProductEtimDynamicElementType>,
    pub udx_edxf_product_features_mc: Option<UdxEdxfProductFeaturesMcElementType>,
    pub udx_edxf_product_characteristics: Option<UdxEdxfProductCharacteristicsElementType>,
}
#[derive(Debug)]
pub struct ProductReferenceElementType {
    pub type_: ProductReferencetypeType,
    pub quantity: Option<i32>,
    pub prod_id_to: String,
    pub catalog_id: Option<String>,
    pub catalog_version: Option<String>,
    pub reference_descr: Vec<DtMlstringType>,
}
#[derive(Debug)]
pub struct ProductLogisticDetailsElementType {
    pub customs_tariff_number: Vec<CustomsTariffNumberElementType>,
    pub statistics_factor: Option<f64>,
    pub country_of_origin: Vec<String>,
}
#[derive(Debug)]
pub enum TupdateProductsProductmodeType {
    Delete,
    New,
    Update,
}
#[derive(Debug)]
pub enum TupdatePricesProductmodeType {
    Update,
}
#[derive(Debug)]
pub enum TnewProductdataProductmodeType {
    New,
}
#[derive(Debug)]
pub struct UdxProductdataType {
    pub udx_edxf_mime_info: Option<UdxEdxfMimeInfoElementType>,
    pub udx_edxf_manufacturer_acronym: Option<String>,
    pub udx_edxf_description_very_short: Vec<DtMlstringType>,
    pub udx_edxf_brand_name: Option<String>,
    pub udx_edxf_tender_text: Vec<DtMlstringType>,
    pub udx_edxf_valid_from: Option<String>,
    pub udx_edxf_expiration_date: Option<String>,
    pub udx_edxf_product_series: Vec<DtMlstringType>,
    pub udx_edxf_product_variation: Vec<DtMlstringType>,
    pub udx_edxf_predecessor_pid: Vec<String>,
    pub udx_edxf_country_branch_numbers: Option<UdxEdxfCountryBranchNumbersElementType>,
    pub udx_edxf_country_branch_supplier_ids: Option<UdxEdxfCountryBranchSupplierIdsElementType>,
    pub udx_edxf_product_etim_dynamic: Option<UdxEdxfProductEtimDynamicElementType>,
    pub udx_edxf_product_features_mc: Option<UdxEdxfProductFeaturesMcElementType>,
    pub udx_edxf_product_characteristics: Option<UdxEdxfProductCharacteristicsElementType>,
}
#[derive(Debug)]
pub enum DtLangType {
    Aar,
    Abk,
    Ace,
    Ach,
    Ada,
    Afa,
    Afh,
    Afr,
    Aka,
    Akk,
    Alb,
    Ale,
    Alg,
    Amh,
    Ang,
    Apa,
    Ara,
    Arc,
    Arm,
    Arn,
    Arp,
    Art,
    Arw,
    Asm,
    Ath,
    Aus,
    Ava,
    Ave,
    Awa,
    Aym,
    Aze,
    Bad,
    Bai,
    Bak,
    Bal,
    Bam,
    Ban,
    Baq,
    Bas,
    Bat,
    Bej,
    Bel,
    Bem,
    Ben,
    Ber,
    Bho,
    Bih,
    Bik,
    Bin,
    Bis,
    Bla,
    Bnt,
    Bod,
    Bos,
    Bra,
    Bre,
    Btk,
    Bua,
    Bug,
    Bul,
    Bur,
    Cad,
    Cai,
    Car,
    Cat,
    Cau,
    Ceb,
    Cel,
    Ces,
    Cha,
    Chb,
    Che,
    Chg,
    Chi,
    Chk,
    Chm,
    Chn,
    Cho,
    Chp,
    Chr,
    Chu,
    Chv,
    Chy,
    Cmc,
    Cop,
    Cor,
    Cos,
    Cpe,
    Cpf,
    Cpp,
    Cre,
    Crp,
    Cus,
    Cym,
    Cze,
    Dak,
    Dan,
    Day,
    Del,
    Den,
    Deu,
    Dgr,
    Din,
    Div,
    Doi,
    Dra,
    Dua,
    Dum,
    Dut,
    Dyu,
    Dzo,
    Efi,
    Egy,
    Eka,
    Ell,
    Elx,
    Eng,
    Enm,
    Epo,
    Est,
    Eus,
    Ewe,
    Ewo,
    Fan,
    Fao,
    Fas,
    Fat,
    Fij,
    Fin,
    Fiu,
    Fon,
    Fra,
    Fre,
    Frm,
    Fro,
    Fry,
    Ful,
    Fur,
    Gaa,
    Gay,
    Gba,
    Gem,
    Geo,
    Ger,
    Gez,
    Gil,
    Gla,
    Gle,
    Glg,
    Glv,
    Gmh,
    Goh,
    Gon,
    Gor,
    Got,
    Grb,
    Grc,
    Gre,
    Grn,
    Guj,
    Gwi,
    Hai,
    Hau,
    Haw,
    Heb,
    Her,
    Hil,
    Him,
    Hin,
    Hit,
    Hmn,
    Hmo,
    Hrv,
    Hun,
    Hup,
    Hye,
    Iba,
    Ibo,
    Ice,
    Ijo,
    Iku,
    Ile,
    Ilo,
    Ina,
    Inc,
    Ind,
    Ine,
    Ipk,
    Ira,
    Iro,
    Isl,
    Ita,
    Jav,
    Jpn,
    Jpr,
    Jrb,
    Kaa,
    Kab,
    Kac,
    Kal,
    Kam,
    Kan,
    Kar,
    Kas,
    Kat,
    Kau,
    Kaw,
    Kaz,
    Kha,
    Khi,
    Khm,
    Kho,
    Kik,
    Kin,
    Kir,
    Kmb,
    Kok,
    Kom,
    Kon,
    Kor,
    Kos,
    Kpe,
    Kro,
    Kru,
    Kua,
    Kum,
    Kur,
    Kut,
    Lad,
    Lah,
    Lam,
    Lao,
    Lat,
    Lav,
    Lez,
    Lin,
    Lit,
    Lol,
    Loz,
    Ltz,
    Lua,
    Lub,
    Lug,
    Lui,
    Lun,
    Luo,
    Lus,
    Mac,
    Mad,
    Mag,
    Mah,
    Mai,
    Mak,
    Mal,
    Man,
    Mao,
    Map,
    Mar,
    Mas,
    May,
    Mdr,
    Men,
    Mga,
    Mic,
    Min,
    Mis,
    Mkd,
    Mkh,
    Mlg,
    Mlt,
    Mnc,
    Mni,
    Mno,
    Moh,
    Mol,
    Mon,
    Mos,
    Mri,
    Msa,
    Mul,
    Mun,
    Mus,
    Mwr,
    Mya,
    Myn,
    Nah,
    Nai,
    Nau,
    Nav,
    Nbl,
    Nde,
    Ndo,
    Nds,
    Nep,
    New,
    Nia,
    Nic,
    Niu,
    Nld,
    Nno,
    Nob,
    Non,
    Nor,
    Nso,
    Nub,
    Nya,
    Nym,
    Nyn,
    Nyo,
    Nzi,
    Oci,
    Oji,
    Ori,
    Orm,
    Osa,
    Oss,
    Ota,
    Oto,
    Paa,
    Pag,
    Pal,
    Pam,
    Pan,
    Pap,
    Pau,
    Peo,
    Per,
    Phi,
    Phn,
    Pli,
    Pol,
    Pon,
    Por,
    Pra,
    Pro,
    Pus,
    Qaa,
    Que,
    Raj,
    Rap,
    Rar,
    Roa,
    Roh,
    Rom,
    Ron,
    Rum,
    Run,
    Rus,
    Sad,
    Sag,
    Sah,
    Sai,
    Sal,
    Sam,
    San,
    Sas,
    Sat,
    Scc,
    Sco,
    Scr,
    Sel,
    Sem,
    Sga,
    Sgn,
    Shn,
    Sid,
    Sin,
    Sio,
    Sit,
    Sla,
    Slk,
    Slo,
    Slv,
    Sme,
    Smi,
    Smo,
    Sna,
    Snd,
    Snk,
    Sog,
    Som,
    Son,
    Sot,
    Spa,
    Sqi,
    Srd,
    Srp,
    Srr,
    Ssa,
    Ssw,
    Suk,
    Sun,
    Sus,
    Sux,
    Swa,
    Swe,
    Syr,
    Tah,
    Tai,
    Tam,
    Tat,
    Tel,
    Tem,
    Ter,
    Tet,
    Tgk,
    Tgl,
    Tha,
    Tib,
    Tig,
    Tir,
    Tiv,
    Tkl,
    Tli,
    Tmh,
    Tog,
    Ton,
    Tpi,
    Tsi,
    Tsn,
    Tso,
    Tuk,
    Tum,
    Tur,
    Tut,
    Tvl,
    Twi,
    Tyv,
    Uga,
    Uig,
    Ukr,
    Umb,
    Und,
    Urd,
    Uzb,
    Vai,
    Ven,
    Vie,
    Vol,
    Vot,
    Wak,
    Wal,
    War,
    Was,
    Wel,
    Wen,
    Wln,
    Wol,
    Xho,
    Yao,
    Yap,
    Yid,
    Yor,
    Ypk,
    Zap,
    Zen,
    Zha,
    Zho,
    Znd,
    Zul,
}
#[derive(Debug)]
pub enum CatalogDatetimetypeType {
    GenerationDate,
}
#[derive(Debug)]
pub enum SupplierAddresstypeType {
    Supplier,
}
#[derive(Debug)]
pub struct MimeElementType {
    pub mime_source: Vec<DtMlstringType>,
    pub mime_descr: Vec<DtMlstringType>,
    pub mime_alt: Vec<DtMlstringType>,
}
#[derive(Debug)]
pub struct InternationalPidElementType {
    pub type_: Option<String>,
    pub content: String,
}
#[derive(Debug)]
pub struct BuyerPidElementType {
    pub type_: Option<String>,
    pub content: String,
}
#[derive(Debug)]
pub struct SpecialTreatmentClassElementType {
    pub type_: String,
    pub content: String,
}
#[derive(Debug)]
pub struct ProductStatusElementType {
    pub lang: Option<DtLangType>,
    pub type_: ProductStatustypeType,
    pub content: String,
}
#[derive(Debug)]
pub enum ProductTypeElementType {
    Contract,
    License,
    Physical,
    Service,
}
#[derive(Debug)]
pub enum TypeClassificationSystemNameType {
    String(String),
    Dynamic,
}
#[derive(Debug)]
pub struct FeatureElementType {
    pub fname: Vec<DtMlstringType>,
    pub fvalue: Vec<DtMlstringType>,
    pub funit: Option<String>,
    pub fvalue_details: Vec<DtMlstringType>,
}
#[derive(Debug)]
pub enum DtUnitType {
    Ann,
    Be,
    Bg,
    Bo,
    Bx,
    C62,
    Ca,
    Cl,
    Cmk,
    Cmq,
    Cmt,
    Cq,
    Cr,
    Cs,
    Ct,
    D99,
    Day,
    Dr,
    Grm,
    Hur,
    Kg,
    Kgm,
    Ktm,
    Ltr,
    Mgm,
    Mlt,
    Mmk,
    Mmt,
    Mmq,
    Mtr,
    Mtk,
    Mtq,
    Pa,
    Pf,
    Pk,
    Pl,
    Pr,
    Pu,
    Rg,
    Rl,
    Ro,
    Sa,
    Sec,
    Set,
    St,
    Tn,
    Tne,
    Tu,
    Wee,
    Z2,
    Z3,
}
#[derive(Debug)]
pub struct ProductPriceDetailsDatetimeElementType {
    pub type_: ProductPriceDetailsDatetimetypeType,
    pub date: String,
}
#[derive(Debug)]
pub struct ProductPriceElementType {
    pub price_type: String,
    pub price_amount: f64,
    pub price_currency: Option<DtCurrenciesType>,
    pub tax: Option<f64>,
    pub price_factor: Option<f64>,
    pub lower_bound: Option<f64>,
    pub territory: Vec<String>,
}
#[derive(Debug)]
pub struct PriceBaseElementType {
    pub price_unit: DtUnitType,
    pub price_unit_factor: Option<f32>,
}
#[derive(Debug)]
pub struct UdxEdxfMimeInfoElementType {
    pub udx_edxf_mime: Vec<UdxEdxfMimeElementType>,
}
#[derive(Debug)]
pub struct UdxEdxfDiscountGroupElementType {
    pub content: Vec<UdxEdxfDiscountGroupElementTypeContent>,
}
#[derive(Debug)]
pub enum UdxEdxfDiscountGroupElementTypeContent {
    UdxEdxfDiscountGroupManufacturer(String),
    UdxEdxfDiscountGroupSupplier(String),
}
#[derive(Debug)]
pub struct UdxEdxfAdditionalFactorsElementType {
    pub udx_edxf_additional_price_factor: f64,
    pub udx_edxf_additional_factor_info: Vec<DtMlstringType>,
}
#[derive(Debug)]
pub struct UdxEdxfCountryBranchNumbersElementType {
    pub udx_edxf_country_branch_number:
        Vec<UdxEdxfCountryBranchNumbersUdxEdxfCountryBranchNumberElementType>,
}
#[derive(Debug)]
pub struct UdxEdxfCountryBranchSupplierIdsElementType {
    pub udx_edxf_country_branch_supplier_id:
        Vec<UdxEdxfCountryBranchSupplierIdsUdxEdxfCountryBranchSupplierIdElementType>,
}
#[derive(Debug)]
pub struct UdxEdxfPackingUnitsElementType {
    pub udx_edxf_packing_unit: Vec<UdxEdxfPackingUnitElementType>,
}
#[derive(Debug)]
pub struct UdxEdxfProductLogisticDetailsElementType {
    pub udx_edxf_netvolume: Option<f64>,
    pub udx_edxf_netweight: Option<f64>,
    pub udx_edxf_netlength: Option<f64>,
    pub udx_edxf_netwidth: Option<f64>,
    pub udx_edxf_netdepth: Option<f64>,
    pub udx_edxf_netdiameter: Option<f64>,
    pub udx_edxf_region_of_origin: Option<String>,
}
#[derive(Debug)]
pub enum UdxEdxfRohsIndicatorElementType {
    True,
    False,
    Exempt,
}
#[derive(Debug)]
pub struct UdxEdxfReachElementType {
    pub udx_edxf_reach_listdate: Option<String>,
    pub udx_edxf_reach_info: UdxEdxfReachInfoElementType,
    pub udx_edxf_scip_number: Option<String>,
    pub udx_edxf_ufi_code: Option<String>,
}
#[derive(Debug)]
pub struct UdxEdxfSpecialTreatmentClassDetailsElementType {
    pub udx_edxf_hazardous_substances: Vec<UdxEdxfHazardousSubstancesElementType>,
    pub udx_edxf_shipping_name: Option<DtMlstringType>,
    pub udx_edxf_packing_group: Option<UdxEdxfPackingGroupElementType>,
    pub udx_edxf_transport_category: Option<i32>,
    pub udx_edxf_multiplication_factor: Option<i32>,
    pub udx_edxf_limited_quantities: Option<String>,
    pub udx_edxf_excepted_quantities: Option<String>,
    pub udx_edxf_aggregation_state: Option<UdxEdxfAggregationStateElementType>,
    pub udx_edxf_special_provision_id: Vec<String>,
    pub udx_edxf_hazard_class: Vec<UdxEdxfHazardClassElementType>,
    pub udx_edxf_classification_code: Option<String>,
    pub udx_edxf_hazard_label: Vec<String>,
    pub udx_edxf_environmental_hazards: Option<String>,
    pub udx_edxf_tunnel_code: Option<UdxEdxfTunnelCodeElementType>,
    pub udx_edxf_ghs_label_code: Vec<UdxEdxfGhsLabelCodeElementType>,
    pub udx_edxf_ghs_signal_word: Option<UdxEdxfGhsSignalWordElementType>,
    pub udx_edxf_hazard_statement: Vec<String>,
    pub udx_edxf_precautionary_statement: Vec<String>,
    pub udx_edxf_li_ion_tested: Option<String>,
    pub udx_edxf_lithium_amount: Option<f64>,
    pub udx_edxf_battery_energy: Option<f64>,
    pub udx_edxf_nos_274: Option<String>,
    pub udx_edxf_hazard_trigger: Vec<String>,
}
#[derive(Debug)]
pub struct UdxEdxfSurchargeListElementType {
    pub udx_edxf_surcharge: Vec<UdxEdxfSurchargeElementType>,
}
#[derive(Debug)]
pub struct UdxEdxfWarrantyElementType {
    pub udx_edxf_warranty_business: Option<i32>,
    pub udx_edxf_warranty_consumer: Option<i32>,
}
#[derive(Debug)]
pub struct UdxEdxfProductEtimDynamicElementType {
    pub udx_edxf_product_etim_release_date: String,
}
#[derive(Debug)]
pub struct UdxEdxfProductFeaturesMcElementType {
    pub udx_edxf_reference_feature_mc_id: String,
    pub udx_edxf_reference_feature_mc_version: i32,
    pub udx_edxf_bim_status: Option<UdxEdxfBimStatusElementType>,
    pub udx_edxf_feature_mc: Vec<UdxEdxfFeatureMcElementType>,
}
#[derive(Debug)]
pub struct UdxEdxfProductCharacteristicsElementType {
    pub udx_edxf_product_characteristic: Vec<UdxEdxfProductCharacteristicElementType>,
}
#[derive(Debug)]
pub enum ProductReferencetypeType {
    Accessories,
    BaseProduct,
    ConsistsOf,
    Followup,
    Mandatory,
    Similar,
    Select,
    Sparepart,
    Others,
}
#[derive(Debug)]
pub struct CustomsTariffNumberElementType {
    pub customs_number: String,
}
#[derive(Debug)]
pub enum ProductStatustypeType {
    Bargain,
    CoreProduct,
    New,
    NewProduct,
    OldProduct,
    Refurbished,
    Used,
    Others,
}
#[derive(Debug)]
pub enum ProductPriceDetailsDatetimetypeType {
    ValidStartDate,
    ValidEndDate,
}
#[derive(Debug)]
pub struct UdxEdxfMimeElementType {
    pub udx_edxf_mime_source: Vec<DtMlstringType>,
    pub udx_edxf_mime_code: UdxEdxfMimeCodeElementType,
    pub udx_edxf_mime_filename: Vec<DtMlstringType>,
    pub udx_edxf_mime_designation: Vec<DtMlstringType>,
    pub udx_edxf_mime_alt: Vec<DtMlstringType>,
    pub udx_edxf_mime_issue_date: Option<String>,
    pub udx_edxf_mime_expiry_date: Option<String>,
    pub udx_edxf_mime_order: Option<i32>,
}
#[derive(Debug)]
pub struct UdxEdxfCountryBranchNumbersUdxEdxfCountryBranchNumberElementType {
    pub type_: String,
    pub country: String,
    pub content: i32,
}
#[derive(Debug)]
pub struct UdxEdxfCountryBranchSupplierIdsUdxEdxfCountryBranchSupplierIdElementType {
    pub type_: String,
    pub country: String,
    pub content: i32,
}
#[derive(Debug)]
pub struct UdxEdxfPackingUnitElementType {
    pub udx_edxf_quantity_min: f32,
    pub udx_edxf_quantity_max: Option<f32>,
    pub udx_edxf_packing_unit_code: DtPunitType,
    pub udx_edxf_packing_unit_name: Vec<DtMlstringType>,
    pub udx_edxf_package_break: Option<String>,
    pub udx_edxf_packing_parts: Option<i32>,
    pub udx_edxf_volume: Option<f64>,
    pub udx_edxf_weight: Option<f64>,
    pub udx_edxf_length: Option<f64>,
    pub udx_edxf_width: Option<f64>,
    pub udx_edxf_depth: Option<f64>,
    pub udx_edxf_diameter: Option<f64>,
    pub udx_edxf_gtin: Option<String>,
    pub udx_edxf_gs_1128: Option<String>,
}
#[derive(Debug)]
pub enum UdxEdxfReachInfoElementType {
    True,
    False,
    NoData,
}
#[derive(Debug)]
pub struct UdxEdxfHazardousSubstancesElementType {
    pub udx_edxf_un_number: String,
    pub udx_edxf_net_weight_of_hazardous_substance: Option<f64>,
    pub udx_edxf_volume_of_hazardous_substances: Option<f64>,
}
#[derive(Debug)]
pub enum UdxEdxfPackingGroupElementType {
    I,
    Ii,
    Iii,
}
#[derive(Debug)]
pub enum UdxEdxfAggregationStateElementType {
    L,
    S,
    G,
}
#[derive(Debug)]
pub enum UdxEdxfHazardClassElementType {
    _1,
    _21,
    _22,
    _23,
    _3,
    _41,
    _42,
    _43,
    _51,
    _52,
    _61,
    _62,
    _7,
    _8,
    _9,
}
#[derive(Debug)]
pub enum UdxEdxfTunnelCodeElementType {
    A,
    B,
    C,
    D,
    E,
}
#[derive(Debug)]
pub enum UdxEdxfGhsLabelCodeElementType {
    Ghs01,
    Ghs02,
    Ghs03,
    Ghs04,
    Ghs05,
    Ghs06,
    Ghs07,
    Ghs08,
    Ghs09,
}
#[derive(Debug)]
pub enum UdxEdxfGhsSignalWordElementType {
    D,
    W,
}
#[derive(Debug)]
pub struct UdxEdxfSurchargeElementType {
    pub content: Vec<UdxEdxfSurchargeElementTypeContent>,
}
#[derive(Debug)]
pub enum UdxEdxfSurchargeElementTypeContent {
    UdxEdxfSurchargeType(String),
    UdxEdxfSurchargeClass(String),
    UdxEdxfSurchargeManner(UdxEdxfSurchargeMannerElementType),
    UdxEdxfSurchargePercentage(f64),
    UdxEdxfSurchargePriceAmount(f64),
    UdxEdxfSurchargeCalculation(UdxEdxfSurchargeUdxEdxfSurchargeCalculationElementType),
    UdxEdxfMaterialBasis(f64),
    UdxEdxfMaterialBasisWeight(f64),
    UdxEdxfMaterialBasisSurchargeThreshold(f64),
    UdxEdxfMaterialBasisSurchargeShutter(
        UdxEdxfSurchargeUdxEdxfMaterialBasisSurchargeShutterElementType,
    ),
    UdxEdxfMaterialBasisSurchargeCredit(
        UdxEdxfSurchargeUdxEdxfMaterialBasisSurchargeCreditElementType,
    ),
    UdxEdxfMaterialBasisSurchargeTable(DtMlstringType),
}
#[derive(Debug)]
pub enum UdxEdxfBimStatusElementType {
    Ready,
    Test,
    Incomplete,
}
#[derive(Debug)]
pub struct UdxEdxfFeatureMcElementType {
    pub content: Vec<UdxEdxfFeatureMcElementTypeContent>,
}
#[derive(Debug)]
pub enum UdxEdxfFeatureMcElementTypeContent {
    UdxEdxfPortcode(i32),
    UdxEdxfFname(String),
    UdxEdxfFvalue(String),
    UdxEdxfCoordinateX(f32),
    UdxEdxfCoordinateY(f32),
    UdxEdxfCoordinateZ(f32),
    UdxEdxfMatrixValues(UdxEdxfMatrixValuesElementType),
}
#[derive(Debug)]
pub struct UdxEdxfProductCharacteristicElementType {
    pub content: Vec<UdxEdxfProductCharacteristicElementTypeContent>,
}
#[derive(Debug)]
pub enum UdxEdxfProductCharacteristicElementTypeContent {
    UdxEdxfProductCharacteristicCode(String),
    UdxEdxfProductCharacteristicName(DtMlstringType),
    UdxEdxfProductCharacteristicValueBoolean(String),
    UdxEdxfProductCharacteristicValueNumeric(f32),
    UdxEdxfProductCharacteristicValueRangeFrom(f32),
    UdxEdxfProductCharacteristicValueRangeTo(f32),
    UdxEdxfProductCharacteristicValueString(DtMlstringType),
    UdxEdxfProductCharacteristicValueSet(DtMlstringType),
    UdxEdxfProductCharacteristicValueSelect(String),
    UdxEdxfProductCharacteristicValueUnitCode(String),
    UdxEdxfProductCharacteristicReferenceGtin(String),
}
#[derive(Debug)]
pub enum UdxEdxfMimeCodeElementType {
    Md01,
    Md02,
    Md03,
    Md04,
    Md05,
    Md06,
    Md07,
    Md08,
    Md09,
    Md10,
    Md11,
    Md12,
    Md13,
    Md14,
    Md15,
    Md16,
    Md17,
    Md18,
    Md19,
    Md20,
    Md21,
    Md22,
    Md23,
    Md24,
    Md25,
    Md26,
    Md27,
    Md28,
    Md29,
    Md30,
    Md31,
    Md32,
    Md33,
    Md34,
    Md35,
    Md37,
    Md38,
    Md39,
    Md40,
    Md41,
    Md42,
    Md43,
    Md44,
    Md45,
    Md46,
    Md47,
    Md48,
    Md49,
    Md50,
    Md51,
    Md52,
    Md53,
    Md54,
    Md55,
    Md56,
    Md57,
    Md58,
    Md59,
    Md60,
    Md61,
    Md62,
    Md63,
    Md64,
    Md65,
    Md99,
}
#[derive(Debug)]
pub enum DtPunitType {
    Be,
    Bg,
    Bo,
    Bx,
    C62,
    Ca,
    Cl,
    Cq,
    Cr,
    Cs,
    Ct,
    Cy,
    D99,
    Dr,
    Ev,
    Kg,
    Ne,
    Pa,
    Pf,
    Pk,
    Pl,
    Pr,
    Pu,
    Rg,
    Rl,
    Ro,
    Sa,
    Set,
    Tn,
    Tu,
    Wr,
    Z2,
    Z3,
}
#[derive(Debug)]
pub enum UdxEdxfSurchargeMannerElementType {
    Base,
    Cumulated,
}
#[derive(Debug)]
pub enum UdxEdxfSurchargeUdxEdxfSurchargeCalculationElementType {
    _1,
    _2,
}
#[derive(Debug)]
pub enum UdxEdxfSurchargeUdxEdxfMaterialBasisSurchargeShutterElementType {
    _1,
    _2,
}
#[derive(Debug)]
pub enum UdxEdxfSurchargeUdxEdxfMaterialBasisSurchargeCreditElementType {
    _1,
    _2,
}
#[derive(Debug)]
pub struct UdxEdxfMatrixValuesElementType {
    pub udx_edxf_matrix_value: Vec<UdxEdxfMatrixValueElementType>,
}
#[derive(Debug)]
pub struct UdxEdxfMatrixValueElementType {
    pub udx_edxf_matrix_source_value: f32,
    pub udx_edxf_matrix_result_value: f32,
}
