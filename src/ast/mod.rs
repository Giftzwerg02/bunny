pub mod parsed;
pub mod scoped;

use std::fmt::{Debug, Display};

use text_trees::StringTreeNode;

pub trait PrettyPrintable {
    fn pretty_print(&self) -> StringTreeNode;
}
pub trait StageInfo: Display + Debug + Clone + PrettyPrintable {}

pub const FUNC_DEF_KEYWORD: &str = "def";
pub const FUNC_LAMBDA_KEYWORD: &str = "\\";

#[derive(Debug, Clone)]
pub enum Expr<I: StageInfo> {
    Int(Int<I>),
    Float(Float<I>),
    String(Str<I>),
    Color(Color<I>),
    Symbol(Symbol<I>),
    FuncCall(FuncCall<I>),
    Array(Array<I>),
    Dict(Dict<I>),
}

impl<I: StageInfo> Expr<I> {
    pub fn name(&self) -> String {
        match self {
            Expr::Int(_) => format!("int"),
            Expr::Float(_) => format!("float"),
            Expr::String(_) => format!("string"),
            Expr::Color(_) => format!("color"),
            Expr::Symbol(_) => format!("symbol"),
            Expr::FuncCall(func_call) => match func_call {
                FuncCall::Single(f) => format!("func::single({})", f.id),
                FuncCall::List(f) => format!("func::list({})", f.calls.len()),
            },
            Expr::Array(_) => format!("array"),
            Expr::Dict(_) => format!("dict"),
        }
    }

    pub fn info(&self) -> &I {
        match self {
            Expr::Int(int) => &int.info,
            Expr::Float(float) => &float.info,
            Expr::String(str) => &str.info,
            Expr::Color(color) => &color.info,
            Expr::Symbol(symbol) => &symbol.info,
            Expr::FuncCall(func_call) => func_call.info(),
            Expr::Array(array) => &array.info,
            Expr::Dict(dict) => &dict.info,
        }
    }

    pub fn as_code(&self) -> String {
        match self {
            Expr::Int(int) => int.as_code(),
            Expr::Float(float) => float.as_code(),
            Expr::String(str) => str.as_code(),
            Expr::Color(color) => color.as_code(),
            Expr::Symbol(symbol) => symbol.as_code(),
            Expr::FuncCall(func_call) => func_call.as_code(),
            Expr::Array(array) => array.as_code(),
            Expr::Dict(dict) => dict.as_code(),
        }
    }

    /// Recursively maps the StageInfo of this expression and all its children
    /// from type I to type O using the provided closure f.
    pub fn map_stage<O: StageInfo, F>(self, f: &mut F) -> Expr<O>
    where
        F: FnMut(I) -> O,
    {
        match self {
            Expr::Int(int) => Expr::Int(int.map_stage(f)),
            Expr::Float(float) => Expr::Float(float.map_stage(f)),
            Expr::String(str) => Expr::String(str.map_stage(f)),
            Expr::Color(color) => Expr::Color(color.map_stage(f)),
            Expr::Symbol(symbol) => Expr::Symbol(symbol.map_stage(f)),
            Expr::FuncCall(func_call) => Expr::FuncCall(func_call.map_stage(f)),
            Expr::Array(array) => Expr::Array(array.map_stage(f)),
            Expr::Dict(dict) => Expr::Dict(dict.map_stage(f)),
        }
    }
}

impl<I: StageInfo> PrettyPrintable for Expr<I> {
    fn pretty_print(&self) -> StringTreeNode {
        match self {
            Expr::Int(int) => int.pretty_print(),
            Expr::Float(float) => float.pretty_print(),
            Expr::String(str) => str.pretty_print(),
            Expr::Color(color) => color.pretty_print(),
            Expr::Symbol(symbol) => symbol.pretty_print(),
            Expr::FuncCall(func_call) => func_call.pretty_print(),
            Expr::Array(array) => array.pretty_print(),
            Expr::Dict(dict) => dict.pretty_print(),
        }
    }
}

impl<I: StageInfo> Display for Expr<I> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Int(int) => {
                write!(f, "{int}")
            }
            Expr::Float(float) => {
                write!(f, "{float}")
            }
            Expr::String(str) => {
                write!(f, "{str}")
            }
            Expr::Color(color) => {
                write!(f, "{color}")
            }
            Expr::Symbol(symbol) => {
                write!(f, "{symbol}")
            }
            Expr::FuncCall(func_call_list) => {
                write!(f, "{func_call_list}")
            }
            Expr::Array(array) => {
                write!(f, "{array}")
            }
            Expr::Dict(dict) => {
                write!(f, "{dict}")
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct Int<I: StageInfo> {
    pub value: u64,
    pub info: I,
}

impl<I: StageInfo> PrettyPrintable for Int<I> {
    fn pretty_print(&self) -> StringTreeNode {
        StringTreeNode::new(format!("{self}"))
    }
}

impl<I: StageInfo> Int<I> {
    pub fn new(value: u64, info: I) -> Self {
        Self { value, info }
    }

    pub fn as_code(&self) -> String {
        format!("{}", self.value)
    }

    /// Maps the StageInfo from type I to type O using the provided closure f.
    pub fn map_stage<O: StageInfo, F: FnMut(I) -> O>(self, f: &mut F) -> Int<O> {
        Int {
            value: self.value,
            // Apply the mapping function f to the StageInfo
            info: f(self.info),
        }
    }
}

impl<I: StageInfo> Display for Int<I> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Int({}, {})", self.value, self.info)
    }
}

#[derive(Debug, Clone)]
pub struct Float<I: StageInfo> {
    pub value: f64,
    pub info: I,
}

impl<I: StageInfo> PrettyPrintable for Float<I> {
    fn pretty_print(&self) -> StringTreeNode {
        StringTreeNode::new(format!("{self}"))
    }
}

impl<I: StageInfo> Float<I> {
    pub fn new(value: f64, info: I) -> Self {
        Self { value, info }
    }

    pub fn as_code(&self) -> String {
        format!("{}f", self.value)
    }

    /// Maps the StageInfo from type I to type O using the provided closure f.
    pub fn map_stage<O: StageInfo, F: FnMut(I) -> O>(self, f: &mut F) -> Float<O> {
        Float {
            value: self.value,
            // Apply the mapping function f to the StageInfo
            info: f(self.info),
        }
    }
}

impl<I: StageInfo> Display for Float<I> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Float({}, {})", self.value, self.info)
    }
}

#[derive(Debug, Clone)]
pub struct Str<I: StageInfo> {
    pub value: String,
    pub info: I,
}

impl<I: StageInfo> PrettyPrintable for Str<I> {
    fn pretty_print(&self) -> StringTreeNode {
        StringTreeNode::new(format!("{self}"))
    }
}

impl<I: StageInfo> Str<I> {
    pub fn new(value: String, info: I) -> Self {
        Self { value, info }
    }

    pub fn as_code(&self) -> String {
        format!("\"{}\"", self.value)
    }

    /// Maps the StageInfo from type I to type O using the provided closure f.
    pub fn map_stage<O: StageInfo, F: FnMut(I) -> O>(self, f: &mut F) -> Str<O> {
        Str {
            value: self.value,
            // Apply the mapping function f to the StageInfo
            info: f(self.info),
        }
    }
}

impl<I: StageInfo> Display for Str<I> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Str({}, {})", self.value, self.info)
    }
}

#[derive(Debug, Clone)]
pub struct Color<I: StageInfo> {
    pub r: u8,
    pub g: u8,
    pub b: u8,
    pub info: I,
}

impl<I: StageInfo> PrettyPrintable for Color<I> {
    fn pretty_print(&self) -> StringTreeNode {
        StringTreeNode::new(format!("{self}"))
    }
}

impl<I: StageInfo> Color<I> {
    pub fn new(r: u8, g: u8, b: u8, info: I) -> Self {
        Self { r, g, b, info }
    }

    pub fn as_code(&self) -> String {
        format!("#{:02x}{:02x}{:02x}", self.r, self.g, self.b)
    }

    /// Maps the StageInfo from type I to type O using the provided closure f.
    pub fn map_stage<O: StageInfo, F: FnMut(I) -> O>(self, f: &mut F) -> Color<O> {
        Color {
            r: self.r,
            g: self.g,
            b: self.b,
            // Apply the mapping function f to the StageInfo
            info: f(self.info),
        }
    }
}

impl<I: StageInfo> Display for Color<I> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Color(#{:02x}{:02x}{:02x}, {})",
            self.r, self.g, self.b, self.info
        )
    }
}

#[derive(Debug, Clone)]
pub struct Symbol<I: StageInfo> {
    pub value: String,
    pub info: I,
}

impl<I: StageInfo> PrettyPrintable for Symbol<I> {
    fn pretty_print(&self) -> StringTreeNode {
        StringTreeNode::new(format!("{self}"))
    }
}

impl<I: StageInfo> Symbol<I> {
    pub fn new(value: String, info: I) -> Self {
        Self { value, info }
    }

    pub fn as_code(&self) -> String {
        format!("{}", self.value)
    }

    /// Maps the StageInfo from type I to type O using the provided closure f.
    pub fn map_stage<O: StageInfo, F: FnMut(I) -> O>(self, f: &mut F) -> Symbol<O> {
        Symbol {
            value: self.value,
            // Apply the mapping function f to the StageInfo
            info: f(self.info),
        }
    }
}

impl<I: StageInfo> Display for Symbol<I> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Symbol({}, {})", self.value, self.info)
    }
}

#[derive(Debug, Clone)]
pub enum FuncCall<I: StageInfo> {
    Single(FuncCallSingle<I>),
    List(FuncCallList<I>),
}

impl<I: StageInfo> FuncCall<I> {
    pub fn info(&self) -> &I {
        match self {
            FuncCall::Single(func_call_single) => &func_call_single.info,
            FuncCall::List(func_call_list) => &func_call_list.info,
        }
    }

    pub fn as_code(&self) -> String {
        match self {
            FuncCall::Single(func_call_single) => func_call_single.as_code(),
            FuncCall::List(func_call_list) => func_call_list.as_code(),
        }
    }

    /// Recursively maps the StageInfo from type I to type O using the provided closure f.
    pub fn map_stage<O: StageInfo, F>(self, f: &mut F) -> FuncCall<O>
    where
        F: FnMut(I) -> O,
    {
        match self {
            FuncCall::Single(func_call_single) => FuncCall::Single(func_call_single.map_stage(f)),
            FuncCall::List(func_call_list) => FuncCall::List(func_call_list.map_stage(f)),
        }
    }
}

impl<I: StageInfo> PrettyPrintable for FuncCall<I> {
    fn pretty_print(&self) -> StringTreeNode {
        match self {
            FuncCall::Single(func_call_single) => StringTreeNode::with_child_nodes(
                "func_call".to_string(),
                vec![func_call_single.pretty_print()].into_iter(),
            ),
            FuncCall::List(func_call_list) => StringTreeNode::with_child_nodes(
                "func_call".to_string(),
                vec![func_call_list.pretty_print()].into_iter(),
            ),
        }
    }
}

#[derive(Debug, Clone)]
pub struct FuncCallList<I: StageInfo> {
    pub calls: Vec<FuncCall<I>>,
    pub info: I,
}

impl<I: StageInfo> PrettyPrintable for FuncCallList<I> {
    fn pretty_print(&self) -> StringTreeNode {
        let mut children = vec![];
        children.push(StringTreeNode::new(format!("{}", self.info)));

        for call in &self.calls {
            children.push(call.pretty_print());
        }

        StringTreeNode::with_child_nodes("func_call_list".to_string(), children.into_iter())
    }
}

impl<I: StageInfo> FuncCallList<I> {
    pub fn new(calls: Vec<FuncCall<I>>, info: I) -> Self {
        Self { calls, info }
    }

    pub fn as_code(&self) -> String {
        let calls = self
            .calls
            .iter()
            .map(|call| call.as_code())
            .collect::<Vec<String>>()
            .join("\n");
        format!("(\n{calls}\n)")
    }

    /// Recursively maps the StageInfo from type I to type O using the provided closure f.
    pub fn map_stage<O: StageInfo, F>(self, f: &mut F) -> FuncCallList<O>
    where
        F: FnMut(I) -> O,
    {
        // Recursively map the StageInfo in each function call
        let new_calls = self
            .calls
            .into_iter()
            .map(|call| call.map_stage(f))
            .collect();
        // Apply the mapping function f to the StageInfo of this node
        let new_info = f(self.info);

        FuncCallList {
            calls: new_calls,
            info: new_info,
        }
    }
}

impl<I: StageInfo> Display for FuncCallList<I> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "FuncCallList({:?}, {})", self.calls, self.info)
    }
}

#[derive(Debug, Clone)]
pub struct FuncCallSingle<I: StageInfo> {
    pub id: Symbol<I>,
    pub args: Vec<Argument<I>>,
    pub info: I,
}

impl<I: StageInfo> PrettyPrintable for FuncCallSingle<I> {
    fn pretty_print(&self) -> StringTreeNode {
        let mut children = vec![];
        children.push(self.id.pretty_print());

        for arg in &self.args {
            children.push(arg.pretty_print());
        }

        children.push(self.info.pretty_print());

        StringTreeNode::with_child_nodes(
            format!("func_call_single: {}", self.info).to_string(),
            children.into_iter(),
        )
    }
}

impl<I: StageInfo> Display for FuncCallSingle<I> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "FuncCallSingle({}, {:?}, {})",
            self.id, self.args, self.info
        )
    }
}

impl<I: StageInfo> FuncCallSingle<I> {
    pub fn new(id: Symbol<I>, args: Vec<Argument<I>>, info: I) -> Self {
        Self { id, args, info }
    }

    pub fn as_code(&self) -> String {
        let args = self
            .args
            .iter()
            .map(|arg| arg.as_code())
            .collect::<Vec<_>>()
            .join(" ");
        format!("({} {})", self.id.as_code(), args)
    }

    pub fn is_def(&self) -> bool {
        return self.id.value == FUNC_DEF_KEYWORD;
    }

    pub fn is_lambda(&self) -> bool {
        return self.id.value == FUNC_LAMBDA_KEYWORD;
    }

    /// Recursively maps the StageInfo from type I to type O using the provided closure f.
    pub fn map_stage<O: StageInfo, F>(self, f: &mut F) -> FuncCallSingle<O>
    where
        F: FnMut(I) -> O,
    {
        // Recursively map the StageInfo in the function identifier
        let new_id = self.id.map_stage(f);
        // Recursively map the StageInfo in each argument
        let new_args = self
            .args
            .into_iter()
            .map(|arg| arg.map_stage(f))
            .collect();
        // Apply the mapping function f to the StageInfo of this node
        let new_info = f(self.info);

        FuncCallSingle {
            id: new_id,
            args: new_args,
            info: new_info,
        }
    }
}

impl<I: StageInfo> Display for FuncCall<I> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FuncCall::Single(func_call_single) => {
                write!(f, "FuncCall::Single({func_call_single})")
            }
            FuncCall::List(func_call_list) => {
                write!(f, "FuncCall::List({func_call_list})")
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum Argument<I: StageInfo> {
    Positional(Expr<I>),
    Named(NamedArgument<I>),
}

impl<I: StageInfo> Argument<I> {
    pub fn info(&self) -> &I {
        match self {
            Argument::Positional(expr) => expr.info(),
            Argument::Named(named_argument) => &named_argument.info,
        }
    }

    pub fn as_code(&self) -> String {
        match self {
            Argument::Positional(positional_argument) => positional_argument.as_code(),
            Argument::Named(named_argument) => named_argument.as_code(),
        }
    }

    /// Recursively maps the StageInfo from type I to type O using the provided closure f.
    pub fn map_stage<O: StageInfo, F>(self, f: &mut F) -> Argument<O>
    where
        F: FnMut(I) -> O,
    {
        match self {
            Argument::Positional(expr) => Argument::Positional(expr.map_stage(f)),
            Argument::Named(named_argument) => Argument::Named(named_argument.map_stage(f)),
        }
    }
}

impl<I: StageInfo> PrettyPrintable for Argument<I> {
    fn pretty_print(&self) -> StringTreeNode {
        match self {
            Argument::Positional(positional_argument) => StringTreeNode::with_child_nodes(
                "argument".to_string(),
                vec![positional_argument.pretty_print()].into_iter(),
            ),
            Argument::Named(named_argument) => StringTreeNode::with_child_nodes(
                "argument".to_string(),
                vec![named_argument.pretty_print()].into_iter(),
            ),
        }
    }
}

impl<I: StageInfo> Display for Argument<I> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Argument::Positional(positional_argument) => {
                write!(f, "{positional_argument}")
            }
            Argument::Named(named_argument) => {
                write!(f, "{named_argument}")
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct NamedArgument<I: StageInfo> {
    pub name: Symbol<I>,
    pub value: Box<Expr<I>>,
    pub info: I,
}

impl<I: StageInfo> PrettyPrintable for NamedArgument<I> {
    fn pretty_print(&self) -> StringTreeNode {
        let mut children = vec![];
        children.push(self.name.pretty_print());
        children.push(self.value.pretty_print());
        children.push(self.info.pretty_print());
        StringTreeNode::with_child_nodes("named_argument".to_string(), children.into_iter())
    }
}

impl<I: StageInfo> NamedArgument<I> {
    pub fn new(name: Symbol<I>, value: Expr<I>, info: I) -> Self {
        Self {
            name,
            value: Box::new(value),
            info,
        }
    }

    pub fn as_code(&self) -> String {
        format!("{}: {}", self.name.as_code(), self.value.as_code())
    }

    /// Recursively maps the StageInfo from type I to type O using the provided closure f.
    pub fn map_stage<O: StageInfo, F>(self, f: &mut F) -> NamedArgument<O>
    where
        F: FnMut(I) -> O,
    {
        // Recursively map the StageInfo in the argument name
        let new_name = self.name.map_stage(f);
        // Recursively map the StageInfo in the argument value
        let new_value = Box::new(self.value.map_stage(f));
        // Apply the mapping function f to the StageInfo of this node
        let new_info = f(self.info);

        NamedArgument {
            name: new_name,
            value: new_value,
            info: new_info,
        }
    }
}

impl<I: StageInfo> Display for NamedArgument<I> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "NamedArgument({}, {}, {})",
            self.name, self.value, self.info
        )
    }
}

#[derive(Debug, Clone)]
pub struct Array<I: StageInfo> {
    pub value: Vec<Expr<I>>,
    pub info: I,
}

impl<I: StageInfo> PrettyPrintable for Array<I> {
    fn pretty_print(&self) -> StringTreeNode {
        let mut children: Vec<_> = self.value.iter().map(|e| e.pretty_print()).collect();
        children.push(self.info.pretty_print());
        StringTreeNode::with_child_nodes("array".to_string(), children.into_iter())
    }
}

impl<I: StageInfo> Array<I> {
    pub fn new(value: Vec<Expr<I>>, info: I) -> Self {
        Self { value, info }
    }

    pub fn as_code(&self) -> String {
        let entries = self
            .value
            .iter()
            .map(|e| e.as_code())
            .collect::<Vec<_>>()
            .join(" ");

        format!("[{entries}]")
    }

    /// Recursively maps the StageInfo from type I to type O using the provided closure f.
    pub fn map_stage<O: StageInfo, F>(self, f: &mut F) -> Array<O>
    where
        F: FnMut(I) -> O,
    {
        // Recursively map the StageInfo in each element
        let new_value = self
            .value
            .into_iter()
            .map(|expr| expr.map_stage(f))
            .collect();
        // Apply the mapping function f to the StageInfo of this node
        let new_info = f(self.info);

        Array {
            value: new_value,
            info: new_info,
        }
    }
}

impl<I: StageInfo> Display for Array<I> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Array({:?}, {})", self.value, self.info)
    }
}

#[derive(Debug, Clone)]
pub struct Dict<I: StageInfo> {
    pub value: Vec<DictEntry<I>>,
    pub info: I,
}

impl<I: StageInfo> PrettyPrintable for Dict<I> {
    fn pretty_print(&self) -> StringTreeNode {
        let mut children: Vec<_> = self.value.iter().map(|e| e.pretty_print()).collect();
        children.push(self.info.pretty_print());
        StringTreeNode::with_child_nodes("array".to_string(), children.into_iter())
    }
}

impl<I: StageInfo> Dict<I> {
    pub fn new(value: Vec<DictEntry<I>>, info: I) -> Self {
        Self { value, info }
    }

    pub fn as_code(&self) -> String {
        if self.value.len() == 0 {
            return "[:]".to_owned();
        }

        let entries = self
            .value
            .iter()
            .map(|e| e.as_code())
            .collect::<Vec<_>>()
            .join(" ");

        format!("[{entries}]")
    }

    /// Recursively maps the StageInfo from type I to type O using the provided closure f.
    pub fn map_stage<O: StageInfo, F>(self, f: &mut F) -> Dict<O>
    where
        F: FnMut(I) -> O,
    {
        // Recursively map the StageInfo in each entry
        let new_value = self
            .value
            .into_iter()
            .map(|entry| entry.map_stage(f))
            .collect();
        // Apply the mapping function f to the StageInfo of this node
        let new_info = f(self.info);

        Dict {
            value: new_value,
            info: new_info,
        }
    }
}

impl<I: StageInfo> Display for Dict<I> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Dict({:?}, {})", self.value, self.info)
    }
}

#[derive(Debug, Clone)]
pub struct DictEntry<I: StageInfo> {
    pub key: Expr<I>,
    pub value: Expr<I>,
    pub info: I,
}

impl<I: StageInfo> PrettyPrintable for DictEntry<I> {
    fn pretty_print(&self) -> StringTreeNode {
        let mut children = vec![];
        children.push(StringTreeNode::with_child_nodes(
            "key".to_string(),
            vec![self.key.pretty_print()].into_iter(),
        ));
        children.push(StringTreeNode::with_child_nodes(
            "value".to_string(),
            vec![self.value.pretty_print()].into_iter(),
        ));
        children.push(StringTreeNode::with_child_nodes(
            "info".to_string(),
            vec![self.info.pretty_print()].into_iter(),
        ));
        StringTreeNode::with_child_nodes("dict_entry".to_string(), children.into_iter())
    }
}

impl<I: StageInfo> DictEntry<I> {
    pub fn new(key: Expr<I>, value: Expr<I>, info: I) -> Self {
        Self { key, value, info }
    }

    pub fn as_code(&self) -> String {
        format!("{}: {}", self.key.as_code(), self.value.as_code())
    }

    /// Recursively maps the StageInfo from type I to type O using the provided closure f.
    pub fn map_stage<O: StageInfo, F>(self, f: &mut F) -> DictEntry<O>
    where
        F: FnMut(I) -> O,
    {
        // Recursively map the StageInfo in the key
        let new_key = self.key.map_stage(f);
        // Recursively map the StageInfo in the value
        let new_value = self.value.map_stage(f);
        // Apply the mapping function f to the StageInfo of this node
        let new_info = f(self.info);

        DictEntry {
            key: new_key,
            value: new_value,
            info: new_info,
        }
    }
}

impl<I: StageInfo> Display for DictEntry<I> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "DictEntry({}, {}, {})", self.key, self.value, self.info)
    }
}
 