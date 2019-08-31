//! Generate FFI wrappers that catch unwindings from C into Rust with a specific
//! payload and re-raises them in Rust as panics with the same payload

/// Configuration builder
pub struct Builder {
    cxx_headers: Vec<String>,
    input_rust_file: std::path::PathBuf,
    name: String,
    skip_fn: Box<dyn Fn(&str) -> bool>,
}

pub struct Output {
    pub rust: String,
    pub cxx: String,
}

impl Builder {
    /// Create a new configuration, where `name` is the name of the library, and
    /// `input_rust_file` is a path to the file containing the extern
    /// declarations.
    pub fn new(name: &str, input_rust_file: &std::path::Path) -> Self {
        Self {
            name: name.to_string(),
            input_rust_file: input_rust_file.to_path_buf(),
            cxx_headers: Vec::default(),
            skip_fn: Box::new(|_| false),
        }
    }
    /// C++ header files required by the library wrappers
    pub fn header(&mut self, header: &str) -> &mut Self {
        self.cxx_headers.push(header.to_string());
        self
    }
    /// Extern declarations that should not get wrappers generated
    pub fn skip_fn<F: Fn(&str) -> bool + 'static>(&mut self, f: F) -> &mut Self {
        self.skip_fn = Box::new(f);
        self
    }
    /// Generate a C++ and a Rust file containing the wrappers.
    ///
    /// The C++ file should be compiled, e.g., using the cc crate, and linked.
    /// The Rust file can be `included!` into your library.
    pub fn generate(self) -> Output {
        let file_src = std::fs::read_to_string(&self.input_rust_file).unwrap_or_else(|e| {
            panic!(
                "failed to read `{}` with errro: `{}`",
                self.input_rust_file.display(),
                e
            )
        });

        let file = syn::parse_file(&file_src).unwrap_or_else(|e| {
            panic!(
                "failed to parse file `{}` with error `{}`",
                self.input_rust_file.display(),
                e
            )
        });

        fn append_items(items: &[syn::Item], fns: &mut Vec<syn::ForeignItemFn>) {
            for item in items {
                match item {
                    syn::Item::ForeignMod(extern_block) => {
                        for item in &extern_block.items {
                            if let syn::ForeignItem::Fn(v) = item {
                                fns.push(v.clone());
                            }
                        }
                    }
                    syn::Item::Mod(m) if m.content.is_some() => {
                        append_items(&m.content.as_ref().unwrap().1, fns)
                    }
                    _ => (),
                }
            }
        }

        let mut fns = Vec::new();
        append_items(&file.items, &mut fns);
        let fns: Vec<_> = fns
            .iter()
            .filter(|v| !(self.skip_fn)(&v.sig.ident.to_string()))
            .collect();

        Output {
            rust: self.rust_wrappers(fns.as_slice()),
            cxx: self.cxx_wrappers(fns.as_slice()),
        }
    }

    fn rust_wrappers(&self, fns: &[&syn::ForeignItemFn]) -> String {
        let mut wrappers = Vec::new();
        wrappers.push(rust_wrapper_header());
        for fun in fns {
            wrappers.push(rust_wrapper(fun));
        }
        wrappers.join("\n")
    }

    fn cxx_wrappers(&self, fns: &[&syn::ForeignItemFn]) -> String {
        let mut wrappers = Vec::new();
        wrappers.push(cxx_wrapper_header(&self));
        for fun in fns {
            wrappers.push(cxx_wrapper(&self, fun));
        }
        wrappers.join("\n")
    }
}

fn rust_wrapper(fun: &syn::ForeignItemFn) -> String {
    let attrs = &fun.attrs;
    let vis = &fun.vis;
    let wrapper_name = &fun.sig.ident;
    let c_wrapper_name = syn::Ident::new(
        &format!("{}_cxx", wrapper_name),
        proc_macro2::Span::call_site(),
    );
    let inputs = &fun.sig.inputs;
    let args = &fun
        .sig
        .inputs
        .iter()
        .map(|arg| {
            if let syn::FnArg::Typed(syn::PatType { pat, .. }) = arg {
                if let syn::Pat::Ident(ref id) = **pat {
                    id.ident.clone()
                } else {
                    panic!("patterns not supported");
                }
            } else {
                panic!("self not supported")
            }
        })
        .collect::<Vec<syn::Ident>>();
    let ret_ty: syn::Type = if let syn::ReturnType::Type(_, t) = &fun.sig.output {
        *t.clone()
    } else {
        syn::parse_str::<syn::Type>("()").unwrap()
    };
    let generics = &fun.sig.generics;
    let output = quote::quote! {
        #(#attrs)*
        #vis
        unsafe fn
        #wrapper_name
        #generics
       (#inputs) -> #ret_ty {
           extern "C" {
               fn
               #c_wrapper_name
               #generics
               (#inputs) -> WrapperResult<#ret_ty>;
           }
           let result = #c_wrapper_name(#(#args),*);
           if result.discriminant == 0 {
               result.value.value
           } else {
               let WrapperError{ buf, len, cap }: WrapperError
                   = result.value.error;
               panic!(String::from_raw_parts(buf, len, cap))
           }
       }
    };
    output.to_string()
}

fn rust_wrapper_header() -> String {
    r#"
        #[repr(C)]
        #[derive(Copy, Clone)]
        struct WrapperError {
            buf: *mut u8, len: usize, cap: usize
        }

        #[repr(C)]
        #[derive(Copy, Clone)]
        union WrapperData<T: Copy> {
            value: T,
            error: WrapperError,
        }

        #[repr(C)]
        #[derive(Copy, Clone)]
        struct WrapperResult<T: Copy> {
            discriminant: u8,
            value: WrapperData<T>
        }
    "#
    .to_string()
}

fn cxx_wrapper(builder: &Builder, fun: &syn::ForeignItemFn) -> String {
    let c_params = &fun
        .sig
        .inputs
        .iter()
        .map(|arg| {
            if let syn::FnArg::Typed(syn::PatType { pat, ty, .. }) = arg {
                if let syn::Pat::Ident(ref id) = **pat {
                    (CType::from_syn(&ty).to_string(), format!("{}", id.ident))
                } else {
                    panic!("patterns not supported");
                }
            } else {
                panic!("self not supported")
            }
        })
        .collect::<Vec<(String, String)>>();
    let c_args = c_params
        .iter()
        .map(|(_, id)| id.clone())
        .collect::<Vec<String>>();
    let c_api_name = format!("{}", &fun.sig.ident);
    let c_api_wrapper_name = format!("{}_cxx", c_api_name);

    let c_ret_ty = CType::from_syn(&match &fun.sig.output {
        syn::ReturnType::Default => syn::parse_str::<syn::Type>("()").unwrap(),
        syn::ReturnType::Type(_, t) => *t.clone(),
    })
    .to_string();

    let value_computation = if c_ret_ty == "void" {
        format!(
            "{c_api_name}({arg_ids}); \
             return WrapperResult<{ret_ty}>::value();",
            ret_ty = c_ret_ty,
            c_api_name = c_api_name,
            arg_ids = c_args.join(",")
        )
    } else {
        format!(
            "return WrapperResult<{ret_ty}>::value({c_api_name}({arg_ids}));",
            ret_ty = c_ret_ty,
            c_api_name = c_api_name,
            arg_ids = c_args.join(",")
        )
    };

    format!(
        r#"
    extern "C" WrapperResult<{ret_ty}> {c_api_wrapper_name}({params}) {{
        try {{
             {value_computation}
        }} catch({name}_rust_wrapper_exception const& e) {{
             return WrapperResult<{ret_ty}>::error(e.error);
        }}
    }}
    "#,
        ret_ty = c_ret_ty,
        name = builder.name,
        c_api_wrapper_name = c_api_wrapper_name,
        params = c_params
            .iter()
            .map(|(ty, id)| format!("{} {}", ty, id))
            .collect::<Vec<String>>()
            .join(","),
        value_computation = value_computation
    )
}

/// Converts a Rust reference type like `&'foo mut x` to a
/// pointer type like `*mut x`.
fn ref_to_ptr(v: &syn::TypeReference) -> syn::Type {
    let mutability = syn::Ident::new(
        if v.mutability.is_some() {
            "mut"
        } else {
            "const"
        },
        proc_macro2::Span::call_site(),
    );
    let elem = *v.elem.clone();
    let s = quote::quote! {
        *#mutability #elem
    }
    .to_string();
    let v = syn::parse_str::<syn::Type>(&s).unwrap();
    if let syn::Type::Ptr(_) = v {
        v
    } else {
        panic!("failed to transform to a pointer")
    }
}

enum CType {
    Ident { id: String },
    Ptr { const_: bool, ty: Box<CType> },
    // TODO: Array
    // TODO: Fn
}

impl CType {
    fn from_syn(rust_ty: &syn::Type) -> Self {
        match rust_ty {
            syn::Type::Ptr(v) => Self::Ptr {
                const_: v.const_token.is_some(),
                ty: Box::new(Self::from_syn(&*v.elem)),
            },
            syn::Type::Reference(v) => Self::from_syn(&ref_to_ptr(v)),
            syn::Type::Never(_) => unimplemented!(),
            syn::Type::BareFn(_) => unimplemented!(),
            syn::Type::Array(_) => unimplemented!(),
            syn::Type::Group(_) => unimplemented!(),
            syn::Type::ImplTrait(_) => unimplemented!(),
            syn::Type::Infer(_) => unimplemented!(),
            syn::Type::Macro(_) => unimplemented!(),
            syn::Type::Paren(_) => unimplemented!(),
            syn::Type::Path(v) => Self::Ident {
                id: rust_path_to_c(v),
            },
            syn::Type::Slice(_) => unimplemented!(),
            syn::Type::TraitObject(_) => unimplemented!(),
            syn::Type::Tuple(v) if v.elems.is_empty() => Self::Ident {
                id: "void".to_string(),
            },
            syn::Type::Tuple(_) => unimplemented!(),
            syn::Type::Verbatim(_) => unimplemented!(),
            _ => panic!("a new syn::Type variant was added to syn"),
        }
    }
}

impl ToString for CType {
    fn to_string(&self) -> String {
        let mut output = String::new();
        let mut current: &Self = self;
        loop {
            match current {
                Self::Ident { id } => {
                    return format!("{} {}", id, output).trim().to_string();
                }
                Self::Ptr { const_, ty } => {
                    output = format!("{} {}", if *const_ { "const *" } else { "*" }, output);
                    current = &*ty;
                }
            }
        }
    }
}

fn rust_path_to_c(p: &syn::TypePath) -> String {
    if p.qself.is_some() {
        unimplemented!();
    }
    if p.path.get_ident().is_none() {
        // FIXME: only paths of len = 1 supported for now
        // TODO: want to probably support `libc::` as a path
        unimplemented!()
    }
    let id = p.path.get_ident().unwrap();
    match id.to_string().as_str() {
        // void:
        "c_void" => "void".to_string(),
        // fixed-width ints:
        "u8" => "uint8_t".to_string(),
        "i8" => "int8_t".to_string(),
        "u16" => "uint16_t".to_string(),
        "i16" => "int16_t".to_string(),
        "u32" => "uint32_t".to_string(),
        "i32" => "int32_t".to_string(),
        "u64" => "uint64_t".to_string(),
        "i64" => "int64_t".to_string(),
        // ptr-sized-ints
        "usize" => "uintptr_t".to_string(),
        "isize" => "intptr_t".to_string(),
        // c types:
        "c_char" => "char".to_string(),
        "c_short" => "short".to_string(),
        "c_int" => "int".to_string(),
        "c_long" => "long".to_string(),
        "c_longlong" => "long long".to_string(),
        "c_uchar" => "unsigned char".to_string(),
        "c_ushort" => "unsigned short".to_string(),
        "c_uint" => "unsigned int".to_string(),
        "c_ulong" => "unsigned long".to_string(),
        "c_ulonglong" => "unsigned long long".to_string(),
        // floating-point (FIXME: should be c_float and c_double)
        "f32" => "float".to_string(),
        "f64" => "double".to_string(),
        // otherwise:
        // TODO: probably want a hook to customize this:
        v => v.to_string(),
    }
}

fn cxx_wrapper_header(builder: &Builder) -> String {
    let includes: Vec<String> = builder
        .cxx_headers
        .iter()
        .map(|i| format!("#include <{}>", i))
        .collect();
    let includes = includes.join("\n");
    format!(
        r#"
        {includes}
        #include <stdexcept>

        struct WrapperError {{
            char* buf;
            uintptr_t len;
            uintptr_t cap;
        }};

        struct mozjpeg_rust_wrapper_exception: std::runtime_error {{
            WrapperError error;
        }};

        template <typename T>
        union WrapperData {{
            T value;
            WrapperError error;
            WrapperData(T v) : value(v) {{}}
            WrapperData(WrapperError e) : error(e) {{}}
        }};

        template <>
        union WrapperData<void> {{
            WrapperError error;
            WrapperData() {{}}
            WrapperData(WrapperError e) : error(e) {{}}
        }};

        template<typename T>
        struct WrapperResult {{
            uint8_t discriminant;
            WrapperData<T> data;
            WrapperResult(uint8_t d, WrapperData<T> v): discriminant(d), data(v) {{}}
            static WrapperResult<T> value(T value) {{
                return WrapperResult(0, WrapperData<T>(value));
            }}
            static WrapperResult<T> error(WrapperError error) {{
                return WrapperResult(1, WrapperData<T>(error));
            }}
        }};

        template<>
        struct WrapperResult<void> {{
            uint8_t discriminant;
            WrapperData<void> data;
            WrapperResult(uint8_t d, WrapperData<void> v): discriminant(d), data(v) {{}}
            static WrapperResult<void> value() {{
                return WrapperResult(0, WrapperData<void>());
            }}
            static WrapperResult<void> error(WrapperError error) {{
                return WrapperResult(1, WrapperData<void>(error));
            }}
        }};
        "#,
        includes = includes
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn rust_wrapper_test() {
        let decl = r#"
            pub fn foo<'a>(x: &'a u8, y: *mut u8) -> u32;
        "#;
        let res = rust_wrapper(&syn::parse_str::<syn::ForeignItemFn>(decl).unwrap());
        let should = r#"pub unsafe fn foo < 'a > ( x : & 'a u8 , y : * mut u8 ) -> -> u32 { extern "C" { fn foo_cxx < 'a > ( x : & 'a u8 , y : * mut u8 ) -> -> u32 ; } let result = foo_cxx ( x , y ) ; if result . discriminant == 0 { result . value . value } else { let WrapperError { { buf , len , cap } } : WrapperError = result . value . error ; panic ! ( String :: from_raw_parts ( buf , len , cap ) ) } }"#;
        assert_eq!(res, should);
    }

    #[test]
    fn cxx_wrapper_test() {
        let decl = r#"
            pub fn foo<'a>(x: &'a u8, y: *mut *const u8) -> u32;
        "#;
        let builder = Builder::new("foobar", std::path::Path::new(""));
        let res = cxx_wrapper(
            &builder,
            &syn::parse_str::<syn::ForeignItemFn>(decl).unwrap(),
        );
        let should = r#"
    extern "C" WrapperResult<uint32_t> foo_cxx(uint8_t const * x,uint8_t const * * y) {
        try {
             return WrapperResult<uint32_t>::value(foo(x,y));
        } catch(foobar_rust_wrapper_exception const& e) {
             return WrapperResult<uint32_t>::error(e.error);
        }
    }
    "#;
        assert_eq!(res, should);
    }

    #[test]
    fn c_types_test() {
        macro_rules! c_type {
            ($lit:literal) => {
                CType::from_syn(&syn::parse_str::<syn::Type>($lit).unwrap()).to_string()
            };
        }
        assert_eq!(c_type!("c_void"), "void");
        assert_eq!(c_type!("()"), "void");

        // fixed-width ints
        assert_eq!(c_type!("i8"), "int8_t");
        assert_eq!(c_type!("i16"), "int16_t");
        assert_eq!(c_type!("i32"), "int32_t");
        assert_eq!(c_type!("i64"), "int64_t");
        assert_eq!(c_type!("u8"), "uint8_t");
        assert_eq!(c_type!("u16"), "uint16_t");
        assert_eq!(c_type!("u32"), "uint32_t");
        assert_eq!(c_type!("u64"), "uint64_t");
        // ptr-sized ints
        assert_eq!(c_type!("usize"), "uintptr_t");
        assert_eq!(c_type!("isize"), "intptr_t");
        // c types
        assert_eq!(c_type!("c_char"), "char");
        assert_eq!(c_type!("c_short"), "short");
        assert_eq!(c_type!("c_int"), "int");
        assert_eq!(c_type!("c_long"), "long");
        assert_eq!(c_type!("c_longlong"), "long long");
        assert_eq!(c_type!("c_uchar"), "unsigned char");
        assert_eq!(c_type!("c_ushort"), "unsigned short");
        assert_eq!(c_type!("c_uint"), "unsigned int");
        assert_eq!(c_type!("c_ulong"), "unsigned long");
        assert_eq!(c_type!("c_ulonglong"), "unsigned long long");
        // floating-point
        assert_eq!(c_type!("f32"), "float");
        assert_eq!(c_type!("f64"), "double");

        // ptr types
        assert_eq!(c_type!("*mut u8"), "uint8_t *");
        assert_eq!(c_type!("*const u8"), "uint8_t const *");
        assert_eq!(c_type!("&mut u8"), "uint8_t *");
        assert_eq!(c_type!("& mut u8"), "uint8_t *");
        assert_eq!(c_type!("&u8"), "uint8_t const *");
        assert_eq!(c_type!("& u8"), "uint8_t const *");
        assert_eq!(c_type!("&'a mut u8"), "uint8_t *");
        assert_eq!(c_type!("&'a mut &'b u8"), "uint8_t const * *");
        assert_eq!(c_type!("&'a &'b mut u8"), "uint8_t * const *");
        assert_eq!(c_type!("*mut &'b u8"), "uint8_t const * *");
        //assert_eq!(c_type!("*const &'b mut u8"), "uint8_t * const *");
        assert_eq!(c_type!("*mut *const u8"), "uint8_t const * *");
        // assert_eq!(c_type!("*const *mut u8"), "uint8_t * const *");
        assert_eq!(c_type!("&'a mut *const u8"), "uint8_t const * *");
        assert_eq!(c_type!("&'a *mut u8"), "uint8_t * const *");
        assert_eq!(c_type!("&'a mut &'b ()"), "void const * *");
    }
}
