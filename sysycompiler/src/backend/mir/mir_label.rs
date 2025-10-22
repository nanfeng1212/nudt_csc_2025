use std::hash::Hash;

#[derive(Debug, Clone)]
pub struct MirLabel( pub String);

impl MirLabel {
    pub fn new(name: String) -> Self { Self(name) }
}

impl PartialEq for MirLabel {
    fn eq(&self, other: &Self) -> bool { 
        self.0 == other.0 
    }
}

impl Eq for MirLabel {}

impl Hash for MirLabel {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) { 
        self.0.hash(state) 
    }
}

impl<T> From<T> for MirLabel
where
    T: AsRef<str>,
{
    fn from(value: T) -> Self { Self(value.as_ref().to_string()) }
}