//! Library to allow the representation and compartmentalization of snippet bodies into segment like those found in [textmate editor](https://macromates.com/manual/en/snippets) and [visual studio code](https://code.visualstudio.com/docs/editor/userdefinedsnippets).
//! Does not actually choose what segment type are to be supported.
//! That is the responsability of another program that implements this library.
//! This is allow this library to be unopininionated about what functionality to include and exclude as well as adhere to the unix philosophy of doing 1 thing and doing 1 well.
#![allow(dead_code)]
use std::rc::{Weak, Rc};
use std::cell::RefCell;
use std::fmt;

/// Representation of a snippet or snippet segment that allows traversal up the ancestors of a snippet('s segment) if any.
pub struct Node {
	pub parent: Option<*mut Node>,
	pub segment: Weak<RefCell<Segment>>,
	pub children: Vec<Node>,
}
impl Clone for Node {
	fn clone(&self) -> Self {
		let mut node = Node {
			parent: None,
			segment: self.segment.clone(),
			children: self.children.iter().map(|child: &Node| {
				child.clone()
			}).collect(),
		};
		let pointer: *mut Node = &mut node;
		for child in node.children.iter_mut() {
			child.parent = Some(pointer);
		}
		let segment = if let Some(segment) = self.segment.upgrade() {
			segment
		} else {
			return node
		};
		let segment = &mut *(&*segment).borrow_mut();
		match segment {
			Segment::Snippet(snippet) => &mut snippet.locations,
			Segment::Tab(tab) => &mut tab.locations,
			#[cfg(feature = "code")]
			Segment::Code(code) => &mut code.locations,
			Segment::Text(text) => &mut text.locations,
			#[cfg(feature = "transform")]
			Segment::Transform(transform) => &mut transform.locations,
			#[cfg(feature = "group-reference")]
			Segment::Group(group) => &mut group.locations,
		}.push(pointer);
		node
	}
}
impl Drop for Node {
	fn drop(&mut self) {
		let self_pointer: *const Node = self;
		let segment = if let Some(segment) = self.segment.upgrade() {
			segment
		} else {
			return
		};
		let segment = &mut *(&*segment).borrow_mut();
		let locations = match segment {
			Segment::Snippet(snippet) => &mut snippet.locations,
			Segment::Tab(tab) => &mut tab.locations,
			#[cfg(feature = "code")]
			Segment::Code(code) => &mut code.locations,
			Segment::Text(text) => &mut text.locations,
			#[cfg(feature = "transform")]
			Segment::Transform(transform) => &mut transform.locations,
			#[cfg(feature = "group-reference")]
			Segment::Group(group) => &mut group.locations,
		};
		let position = if let Some(position) = locations.iter().position(|&location| std::ptr::eq(location, self_pointer)) {
			position
		} else {
			return
		};
		locations.remove(position);
	}
}
impl fmt::Display for Node {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		let segment = if let Some(segment) = self.segment.upgrade() {
			segment
		} else {
			return Ok(())
		};
		let ret = (&*(&*segment).borrow()).fmt(f);
		ret
	}
}
impl Node {
	pub fn reset_children(&mut self) {
		let segment = if let Some(segment) = self.segment.upgrade() {
			segment
		} else {
			return
		};
		let segment = &*(&*segment).borrow();
		self.children = match segment {
			Segment::Snippet(snippet) => {
				&snippet.children
			},
			#[cfg(feature = "group-reference")]
			Segment::Group(group) => {
				&group.children
			},
			Segment::Tab(tab) => {
				tab.field.get_children()
			},
			_ => {
				self.children = vec![];
				return
			},
		}.iter().map(|child: &Rc<RefCell<Segment>>| {
			get_node(child.clone())
		}).collect();
		let pointer: *mut Node = self;
		for child in self.children.iter_mut() {
			child.parent = Some(pointer);
		}
	}
}

///Representation of a portion of a snippet.
pub enum Segment {
	///Nested snippet.
	///Intended to be created from user expanding a snippet within tabs
	Snippet(Snippet),
	///Portion of snippet set by user input
	Tab(Tab),
	///Portion that is the output of an interpreter (computer program that executes source code) being fed source code.
	#[cfg(feature = "code")]
	Code(Code),
	///Regular plain old text.
	Text(Text),
	///Group of sibling segments that are given a name for reuse.
	#[cfg(feature = "group-reference")]
	Group(Group),
	///Text that results from replacing a pattern of another segment with replacement text.
	#[cfg(feature = "transform")]
	Transform(Transform),
}
impl std::cmp::Ord for Segment {
	fn cmp(&self, other: &Self) -> std::cmp::Ordering {
		let self_ord_priority: u8 = match self {
			Segment::Snippet(_) => 0,
			Segment::Tab(self_tab) => {
				if let Segment::Tab(other_tab) = other {
					return self_tab.cmp(other_tab)
				}
				1
			},
			Segment::Text(_) => 2,
			#[cfg(feature = "code")]
			Segment::Code(self_code) => {
				if let Segment::Code(other_code) = other {
					return self_code.cmp(other_code)
				}
				3
			},
			#[cfg(feature = "transform")]
			Segment::Transform(self_transform) => {
				if let Segment::Transform(other_transform) = other {
					return self_transform.cmp(other_transform)
				}
				4
			},
			#[cfg(feature = "group-reference")]
			Segment::Group(self_group) => {
				if let Segment::Group(other_group) = other {
					return self_group.cmp(other_group)
				}
				5
			},
		};
		let other_ord_priority: u8 = match self {
			Segment::Snippet(_) => 0,
			Segment::Tab(_) => 1,
			Segment::Text(_) => 2,
			#[cfg(feature = "code")]
			Segment::Code(_) => 3,
			#[cfg(feature = "transform")]
			Segment::Transform(_) => 4,
			#[cfg(feature = "group-reference")]
			Segment::Group(_) => 5,
		};
		self_ord_priority.cmp(&other_ord_priority)
	}
}
impl std::cmp::PartialOrd for Segment {
	fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
		Some(self.cmp(other))
	}
}
impl std::cmp::PartialEq for Segment {
	fn eq(&self, other: &Self) -> bool {
		match self {
			Segment::Snippet(self_snippet) => {
				if let Segment::Snippet(other_snippet) = other {
					return self_snippet.eq(other_snippet)
				}
			},
			Segment::Tab(self_tab) => {
				if let Segment::Tab(other_tab) = other {
					return self_tab.eq(other_tab)
				}
			},
			#[cfg(feature = "code")]
			Segment::Code(self_code) => {
				if let Segment::Code(other_code) = other {
					return self_code.eq(other_code)
				}
			},
			Segment::Text(self_text) => {
				if let Segment::Text(other_text) = other {
					return self_text.eq(other_text)
				}
			},
			#[cfg(feature = "transform")]
			Segment::Transform(self_transform) => {
				if let Segment::Transform(other_transform) = other {
					return self_transform.eq(other_transform)
				}
			},
			#[cfg(feature = "group-reference")]
			Segment::Group(self_group) => {
				if let Segment::Group(other_group) = other {
					return self_group.eq(other_group)
				}
			},
		};
		false
	}
}
impl std::cmp::Eq for Segment {}
impl fmt::Display for Segment {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			Segment::Snippet(snippet) => snippet.fmt(f),
			Segment::Tab(tab) => tab.fmt(f),
			#[cfg(feature = "code")]
			Segment::Code(code) => code.fmt(f),
			Segment::Text(text) => text.fmt(f),
			#[cfg(feature = "transform")]
			Segment::Transform(transform) => transform.fmt(f),
			#[cfg(feature = "group-reference")]
			Segment::Group(group) => group.fmt(f),
		}
	}
}
///Produce a node from a segment.
pub fn get_node(segment: Rc<RefCell<Segment>>) -> Node {
	let mut node = Node {
		parent: None,
		segment: Rc::downgrade(&segment),
		children: match &*(&*segment).borrow() {
			Segment::Snippet(snippet) => {
				snippet.children.iter().map(|child: &Rc<RefCell<Segment>>| {
					get_node(child.clone())
				}).collect()
			},
			Segment::Tab(tab) => {
				tab.field.get_children().iter().map(|child: &Rc<RefCell<Segment>>| {
					get_node(child.clone())
				}).collect()
			},
			#[cfg(feature = "group-reference")]
			Segment::Group(group) => {
				group.children.iter().map(|child: &Rc<RefCell<Segment>>| {
					get_node(child.clone())
				}).collect()
			},
			_ => vec![],
		},
	};
	let pointer: *mut Node = &mut node;
	for child in node.children.iter_mut() {
		child.parent = Some(pointer);
	}
	let segment = &mut *(&*segment).borrow_mut();
	match segment {
		Segment::Snippet(snippet) => &mut snippet.locations,
		Segment::Tab(tab) => &mut tab.locations,
		#[cfg(feature = "code")]
		Segment::Code(code) => &mut code.locations,
		Segment::Text(text) => &mut text.locations,
		#[cfg(feature = "transform")]
		Segment::Transform(transform) => &mut transform.locations,
		#[cfg(feature = "group-reference")]
		Segment::Group(group) => &mut group.locations,
	}.push(pointer);
	node
}
pub fn find_segment(segments: &mut Vec<Rc<RefCell<Segment>>>, segment: Rc<RefCell<Segment>>) -> Result<Rc<RefCell<Segment>>, Option<(usize, &mut Vec<Rc<RefCell<Segment>>>)>> {
	let search_index = segments.binary_search(&segment);
	match search_index {
		Err(search_index) => Err(Some((search_index, segments))),
		Ok(search_index) => Ok(segments[search_index].clone()),
	}
}
pub fn find_segment_index(segments: &mut Vec<Rc<RefCell<Segment>>>, segment: Rc<RefCell<Segment>>) -> Result<(usize, &mut Vec<Rc<RefCell<Segment>>>), (usize, &mut Vec<Rc<RefCell<Segment>>>)> {
	let search_index = segments.binary_search(&segment);
	match search_index {
		Err(search_index) => Err((search_index, segments)),
		Ok(search_index) => Ok((search_index, segments)),
	}
}

/// Part of a snippet.
pub trait InteractiveSegment: {
	/// Returns string that uniquely identifies the InteractiveSegment's implementor.
	/// If more than one implementor of InteractiveSegment has get_type return the same string then programs using this library can crash or have undefined behavior.
	/// Used to allow operating implementors of only on one or more concrete types within multiple implementors.
	/// Used by [cast_interactive_segment](cast_interactive_segment), [cast_mut_interactive_segment](cast_mut_interactive_segment), [cast_field](cast_field) and [cast_mut_field](cast_mut_field).
	/// Can be used by things like [filter](std::iter::Iterator::filter).
	fn get_type(&self) -> &str;
}
/// Used to cast a type implementing InteractiveSegment into into a concrete type [implementing InteractiveSegment](Snippet::body).
/// Instead equivalent Segment::Interactive should be used in body and Reference::InteractiveSegment should be found in the [snippet references](Snippet::references).
/// This casting will not be mutable.
/// Can be used by things like [filter](std::iter::Iterator::filter) and especially [filter_map](std::iter::Iterator::filter_map).
pub fn cast_interactive_segment<I: InteractiveSegment>(interactive: &dyn InteractiveSegment) -> Option<&I> {
	let null_reference: &I = unsafe {
		std::mem::transmute_copy(&std::ptr::null::<I>())
	};
	if interactive.get_type() != I::get_type(null_reference) {
		return None
	}
	Some(unsafe {
			let (struct_implementing_trait, _): (&I, *const ()) = std::mem::transmute(interactive);
			struct_implementing_trait
	})
}
/// Used to cast a type implementing InteractiveSegment into into a concrete type implementing InteractiveSegment.
/// This casting will be mutable.
/// Can be used by things like [filter](std::iter::Iterator::filter) and especially [filter_map](std::iter::Iterator::filter_map).
pub fn cast_mut_interactive_segment<I: InteractiveSegment>(interactive: &mut dyn InteractiveSegment) -> Option<&mut I> {
	let null_reference: &I = unsafe {
		std::mem::transmute_copy(&std::ptr::null::<I>())
	};
	if interactive.get_type() != I::get_type(null_reference) {
		return None
	}
	Some(unsafe {
			let (struct_implementing_trait, _): (&mut I, *const ()) = std::mem::transmute(interactive);
			struct_implementing_trait
	})
}

/// Part of a snippet that is filled in by user input (like fields in a form).
pub trait Field: InteractiveSegment {
	fn get_children(&self) -> &Vec<Rc<RefCell<Segment>>>;
	fn get_mut_children(&mut self) -> &mut Vec<Rc<RefCell<Segment>>>;
}
/// Used to cast a type implementing Field into into a concrete type implementing Field.
/// This casting will not be mutable.
/// Can be used by things like [filter](std::iter::Iterator::filter) and especially [filter_map](std::iter::Iterator::filter_map).
pub fn cast_field<F: Field>(field: &dyn Field) -> Option<&F> {
	let interactive: &dyn InteractiveSegment = unsafe {
		std::mem::transmute(field)
	};
	cast_interactive_segment::<F>(interactive)
}
/// Used to cast a type implementing Field into into a concrete type implementing Field.
/// This casting will be mutable.
/// Can be used by things like [filter](std::iter::Iterator::filter) and especially [filter_map](std::iter::Iterator::filter_map).
pub fn cast_mut_field<F: Field>(field: &mut dyn Field) -> Option<&mut F> {
	let interactive: &mut dyn InteractiveSegment = unsafe {
		std::mem::transmute(field)
	};
	cast_mut_interactive_segment::<F>(interactive)
}

/// Part of a snippet that is filled in programically (like variables or shell code expansions).
#[cfg(feature = "code")]
pub trait Interpreter: InteractiveSegment {
	/// Runs it's source code through an interpreter and returns the interpreter's output.
	fn evaluate(&self, source_code: &str) -> String;
}
/// Used to cast a type implementing Interpreter into into a concrete type implementing Interpreter.
/// This casting will not be mutable.
/// Can be used by things like [filter](std::iter::Iterator::filter) and especially [filter_map](std::iter::Iterator::filter_map).
#[cfg(feature = "code")]
pub fn cast_interpreter<I: Interpreter>(interpreter: &dyn Interpreter) -> Option<&I> {
	let interactive: &dyn InteractiveSegment = unsafe {
		std::mem::transmute(interpreter)
	};
	cast_interactive_segment::<I>(interactive)
}
/// Used to cast a type implementing Interpreter into into a concrete type implementing Interpreter.
/// This casting will be mutable.
/// Can be used by things like [filter](std::iter::Iterator::filter) and especially [filter_map](std::iter::Iterator::filter_map).
#[cfg(feature = "code")]
pub fn cast_mut_interpreter<I: Interpreter>(interpreter: &mut dyn Interpreter) -> Option<&mut I> {
	let interactive: &mut dyn InteractiveSegment = unsafe {
		std::mem::transmute(interpreter)
	};
	cast_mut_interactive_segment::<I>(interactive)
}

pub struct Snippet {
	///All the code segments.
	#[cfg(feature = "code")]
	pub code: Vec<Rc<RefCell<Segment>>>,
	///All the transform segments.
	#[cfg(feature = "transform")]
	pub transforms: Vec<Rc<RefCell<Segment>>>,
	///All the tab segments.
	pub tabs: Vec<Rc<RefCell<Segment>>>,
	#[cfg(any(feature = "reference", feature = "group-reference"))]
	pub references: Vec<Rc<RefCell<Reference>>>,
	///All the Nodes where this snippet is found if it is nested within another snippet
	pub locations: Vec<*mut Node>,
	pub children: Vec<Rc<RefCell<Segment>>>,
}
impl fmt::Display for Snippet {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		for child in self.children.iter() {
			(&*(&**child).borrow()).fmt(f)?;
		}
		Ok(())
	}
}
impl std::cmp::PartialEq for Snippet {
	fn eq(&self, other: &Self) -> bool {
		std::ptr::eq(self, other)
	}
}
impl Snippet {
	pub fn add(&mut self, segment: Rc<RefCell<Segment>>) {
		if let Err(Some((find_index, field_to_add_to))) = self.find(segment.clone()) {
			field_to_add_to.insert(find_index, segment);
		}
	}
	pub fn find(&mut self, segment: Rc<RefCell<Segment>>) -> Result<Rc<RefCell<Segment>>, Option<(usize, &mut Vec<Rc<RefCell<Segment>>>)>> {
		let field_to_search_in = match &*(&*segment).borrow() {
			Segment::Tab(_) => &mut self.tabs,
			#[cfg(feature = "code")]
			Segment::Code(_) => &mut self.code,
			#[cfg(feature = "transform")]
			Segment::Transform(_) => &mut self.transforms,
			_ => return Err(None),
		};
		find_segment(field_to_search_in, segment)
	}
	fn new_empty() -> Self {
		Snippet {
			///All the code segments.
			#[cfg(feature = "code")]
			code: Vec::new(),
			///All the transform segments.
			#[cfg(feature = "transform")]
			transforms: Vec::new(),
			///All the tab segments.
			tabs: Vec::new(),
			#[cfg(any(feature = "reference", feature = "group-reference"))]
			references: Vec::new(),
			///All the Nodes where this snippet is found if it is nested within another snippet
			locations: Vec::new(),
			children: Vec::new(),
		}
	}
}

///Regular plain old text.
pub struct Text {
	///Reference of this text, if any.
	///If there is none reference then self.reference.upgrade() will return None.
	#[cfg(feature = "reference")]
	pub reference: Weak<RefCell<Reference>>,
	///All the Nodes where this text is found if it is within another snippet.
	pub locations: Vec<*mut Node>,
	///Content of this text.
	pub string: String,
}
impl std::cmp::PartialEq for Text {
	fn eq(&self, other: &Self) -> bool {
		std::ptr::eq(self, other)
	}
}
impl fmt::Display for Text {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		self.string.fmt(f)
	}
}

///Portion of snippet set by user input
pub struct Tab {
	pub num: u8,
	///Content of this tab.
	pub field: Box<dyn Field>,
	///Reference of this tab, if any.
	///If there is none reference then self.reference.upgrade() will return None.
	#[cfg(feature = "reference")]
	pub reference: Weak<RefCell<Reference>>,
	///All the Nodes where this tab is found if it is within another snippet.
	pub locations: Vec<*mut Node>,
}
impl fmt::Display for Tab {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		for child in self.field.get_children().iter() {
			(&*(&**child).borrow()).fmt(f)?;
		}
		Ok(())
	}
}
impl std::cmp::Ord for Tab {
	fn cmp(&self, other: &Self) -> std::cmp::Ordering {
		self.num.cmp(&other.num)
	}
}
impl std::cmp::PartialOrd for Tab {
	fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
		Some(self.cmp(other))
	}
}
impl std::cmp::PartialEq for Tab {
	fn eq(&self, other: &Self) -> bool {
		self.num == other.num
	}
}
impl Eq for Tab {}

///Portion that is the output of an interpreter (computer program that executes source code) being fed source code.
#[cfg(feature = "code")]
pub struct Code {
	///The actual program being run that will produce output.
	pub source_code: String,
	///Result of running source_code through the interpreter.
	pub output: String,
	///The program that runs the source_code to get its output.
	pub interpreter: Box<dyn Interpreter>,
	///Reference of this code, if any.
	///If there is none reference then self.reference.upgrade() will return None.
	#[cfg(feature = "reference")]
	pub reference: Weak<RefCell<Reference>>,
	///All the Nodes where this code is found if it is within another snippet.
	pub locations: Vec<*mut Node>,
}
#[cfg(feature = "code")]
impl std::cmp::Ord for Code {
	fn cmp(&self, other: &Self) -> std::cmp::Ordering {
		let ret = self.interpreter.get_type().cmp(other.interpreter.get_type());
		if ret != std::cmp::Ordering::Equal {
			return ret
		}
		self.source_code.cmp(&other.source_code)
	}
}
#[cfg(feature = "code")]
impl std::cmp::PartialOrd for Code {
	fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
		Some(self.cmp(other))
	}
}
#[cfg(feature = "code")]
impl std::cmp::PartialEq for Code {
	fn eq(&self, other: &Self) -> bool {
		if !self.interpreter.get_type().eq(other.interpreter.get_type()) {
			return false
		}
		self.source_code == other.source_code
	}
}
#[cfg(feature = "code")]
impl Eq for Code {}
#[cfg(feature = "code")]
impl Code {
	pub fn evaluate(&mut self) {
		self.output = self.interpreter.evaluate(&self.source_code);
	}
}
#[cfg(feature = "code")]
impl fmt::Display for Code {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		self.output.fmt(f)
	}
}

///Text that results from replacing a pattern of another segment with replacement text.
#[cfg(feature = "transform")]
pub struct Transform {
	pub pattern: String,
	///What to replace the pattern with.
	pub replacement: String,
	///Influences how pattern is matched.
	#[cfg(feature = "transform-flags")]
	pub flags: String,
	///The text representation of source will be to target of the pattern matching and replacement to produce output.
	pub source: Weak<RefCell<Segment>>,
	///Reference of this transform, if any.
	///If there is none reference then self.reference.upgrade() will return None.
	#[cfg(feature = "reference")]
	pub reference: Weak<RefCell<Reference>>,
	///All the Nodes where this transform is found if it is within another snippet.
	pub locations: Vec<*mut Node>,
	///Result to the pattern matching and replacement.
	pub output: String
}
#[cfg(feature = "transform")]
impl Transform {
	pub fn evaluate(&mut self) {
		unsafe {
			if let Some(eval) = GET_TRANSFORM_OUTPUT {
				eval(self)
			}
		}
	}
}
///The function used to replace pattern matches with replacement text to produce output of a transform
#[cfg(feature = "transform")]
pub static mut GET_TRANSFORM_OUTPUT: Option<fn(&mut Transform)> = None;
#[cfg(feature = "transform")]
impl std::cmp::Ord for Transform {
	fn cmp(&self, other: &Self) -> std::cmp::Ordering {
		let mut ret = self.pattern.cmp(&other.pattern);
		if ret != std::cmp::Ordering::Equal {
			return ret
		}
		ret = self.replacement.cmp(&other.replacement);
		if ret != std::cmp::Ordering::Equal {
			return ret
		}
		#[cfg(feature = "transform-flags")]
		{
			ret = self.flags.cmp(&other.flags);
			if ret != std::cmp::Ordering::Equal {
				return ret
			}
		}
		let self_segment = self.source.upgrade();
		if let None = self_segment {
			if let None = other.source.upgrade() {
				return std::cmp::Ordering::Equal
			}
			return std::cmp::Ordering::Less
		}
		let other_segment = other.source.upgrade();
		if let None = other_segment {
			return std::cmp::Ordering::Greater
		}
		let self_segment = self_segment.unwrap();
		let other_segment = other_segment.unwrap();
		self_segment.cmp(&other_segment)
	}
}
#[cfg(feature = "transform")]
impl std::cmp::PartialOrd for Transform {
	fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
		Some(self.cmp(other))
	}
}
#[cfg(feature = "transform")]
impl std::cmp::PartialEq for Transform {
	fn eq(&self, other: &Self) -> bool {
		let mut ret = self.pattern.eq(&other.pattern);
		if !ret {
			return ret
		}
		ret = self.replacement.eq(&other.replacement);
		if !ret {
			return ret
		}
		#[cfg(feature = "transform-flags")]
		{
			ret = self.flags.eq(&other.flags);
			if !ret {
				return ret
			}
		}
		self.reference.ptr_eq(&other.reference)
	}
}
#[cfg(feature = "transform")]
impl Eq for Transform {}
#[cfg(feature = "transform")]
impl fmt::Display for Transform {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		self.output.fmt(f)
	}
}

/// Snippet portion given unique name for reuse and clarity.
/// Good for reusing text in multiple places.
/// Or a InteractiveSegment that doesn't have a form of unique identification.
#[cfg(any(feature = "reference", feature = "group-reference"))]
pub struct Reference {
	pub name: String,
	pub value: Weak<RefCell<Segment>>,
}
#[cfg(any(feature = "reference", feature = "group-reference"))]
impl std::cmp::PartialEq for Reference {
	fn eq(&self, other: &Self) -> bool {
		self.name == other.name
	}
}
#[cfg(any(feature = "reference", feature = "group-reference"))]
impl std::cmp::Eq for Reference {}
#[cfg(any(feature = "reference", feature = "group-reference"))]
impl std::cmp::PartialOrd for Reference {
	fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
		Some(self.cmp(other))
	}
}
#[cfg(any(feature = "reference", feature = "group-reference"))]
impl std::cmp::Ord for Reference {
	fn cmp(&self, other: &Self) -> std::cmp::Ordering {
		self.name.cmp(&other.name)
	}
}
#[cfg(any(feature = "reference", feature = "group-reference"))]
pub fn find_reference(references: &mut Vec<Rc<RefCell<Reference>>>, reference: Rc<RefCell<Reference>>) -> Result<Rc<RefCell<Reference>>, (usize, &mut Vec<Rc<RefCell<Reference>>>)> {
	let search_index = references.binary_search(&reference);
	match search_index {
		Err(search_index) => Err((search_index, references)),
		Ok(search_index) => Ok(references[search_index].clone()),
	}
}

///Group of sibling segments that are given a name for reuse.
#[cfg(feature = "group-reference")]
pub struct Group {
	pub reference: Weak<RefCell<Reference>>,
	pub locations: Vec<*mut Node>,
	pub children: Vec<Rc<RefCell<Segment>>>,
}
#[cfg(feature = "group-reference")]
impl std::cmp::PartialEq for Group {
	fn eq(&self, other: &Self) -> bool {
		self.reference.ptr_eq(&other.reference)
	}
}
#[cfg(feature = "group-reference")]
impl std::cmp::Eq for Group {}
#[cfg(feature = "group-reference")]
impl std::cmp::PartialOrd for Group {
	fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
		Some(self.cmp(other))
	}
}
#[cfg(feature = "group-reference")]
impl std::cmp::Ord for Group {
	fn cmp(&self, other: &Self) -> std::cmp::Ordering {
		let self_reference = self.reference.upgrade();
		if let None = self_reference {
			if let None = other.reference.upgrade() {
				return std::cmp::Ordering::Equal
			}
			return std::cmp::Ordering::Less
		}
		let other_reference = other.reference.upgrade();
		if let None = other_reference {
			return std::cmp::Ordering::Greater
		}
		let self_reference = self_reference.unwrap();
		let other_reference = other_reference.unwrap();
		self_reference.cmp(&other_reference)
	}
}
#[cfg(feature = "group-reference")]
impl fmt::Display for Group {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		for child in self.children.iter() {
			(&*(&**child).borrow()).fmt(f)?;
		}
		Ok(())
	}
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
    }
}
