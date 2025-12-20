use core::fmt;


#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum PathElement {
    Name(String),
    Inferred
}

impl PathElement {
    pub fn name(&self) -> Option<&String> {
        match self {
            PathElement::Name(s) => Some(s),
            PathElement::Inferred => None,
        }
    }
}

impl From<&String> for PathElement {
    fn from(value: &String) -> Self {
        PathElement::Name(value.to_owned())
    }
}

impl From<String> for PathElement {
    fn from(value: String) -> Self {
        PathElement::Name(value)
    }
}

impl From<&str> for PathElement {
    fn from(value: &str) -> Self {
        PathElement::Name(value.into())
    }
}

impl fmt::Display for PathElement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PathElement::Name(name) => write!(f, "{}", name),
            PathElement::Inferred => write!(f, "_"),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Path {
    elements: Vec<PathElement>
}

impl From<&String> for Path {
    fn from(value: &String) -> Self {
        Path { elements: vec![value.into()] }
    }
}

impl From<String> for Path {
    fn from(value: String) -> Self {
        Path { elements: vec![value.into()] }
    }
}

impl From<&str> for Path {
    fn from(value: &str) -> Self {
        Path { elements: vec![value.into()] }
    }
}

impl Path {
    pub fn inferred() -> Self {
        Path { elements: vec![PathElement::Inferred] }
    }

    pub fn element_count(&self) -> usize {
        self.elements.len()
    }
    
    pub fn is_inferred(&self) -> bool {
        self.elements.iter().any(|e| matches!(e, PathElement::Inferred))
    }
    
    pub fn elements(&self) -> &Vec<PathElement> {
        &self.elements
    }

    pub fn add(&mut self, element: PathElement) {
        if let Some(PathElement::Inferred) = self.elements.last() {
            self.elements.pop();
        }
        self.elements.push(element);
    }
    
    pub fn extend(&mut self, path: Path) {
        if let Some(PathElement::Inferred) = self.elements.last() {
            self.elements.pop();
        }
        self.elements.extend(path.elements);
    }
}

impl fmt::Display for Path {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for element in (&self.elements[1..]).iter().rev() {
            write!(f, "{}::", element)?;
        }
        write!(f, "{}", self.elements[0])
    }
}