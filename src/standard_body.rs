use std::rc::Rc;
use std::fmt;
use std::cell::RefCell;

/// Part of a snippet.
pub trait InteractiveSegment: fmt::Display + fmt::Debug {
	/// Returns string that uniquely identifies the InteractiveSegment's implementor.
	/// If more than one implementor of InteractiveSegment has get_type return the same string then programs using this library can crash or have undefined behavior.
	/// Used to allow operating implementors of only on one or more concrete types within multiple implementors.
	/// Used by [cast_interactive_segment](cast_interactive_segment), [cast_mut_interactive_segment](cast_mut_interactive_segment), [cast_field](cast_field) and [cast_mut_field](cast_mut_field).
	/// Can be used by things like [filter](std::iter::Iterator::filter).
	fn get_type(&self) -> &str;
	/// Indicate whether this InteractiveSegment implementor contains a Segment Vec (thus nested content) with Some(&Vec<Segment>) or not with None.
	/// By default this method will return None.
	/// This is to allow the searching of the Snippet body for segments that are contained within the Snippet's fields (tabs, references, etc).
	/// Snippet segment types that can contain other snippet segments inside them should have implement this method and have it return Some(&Vec<Segment>) with one exception.
	/// Snippet should not implement nested_printed_segments method which returns a Some(&Vec<Segment>) since a nested snippet will not be searched for segments contained within the outer snippet's fields.
	fn nested_printed_segments(&self) -> Option<&Vec<Segment>> {
		None
	}
	fn nested_segments(&self) -> Option<Vec<&Vec<Segment>>> {
		let printed_segments = self.nested_printed_segments()?;
		Some(vec![printed_segments])
	}
}

/// Part of a snippet that is filled in by user input (like fields in a form).
pub trait Field: InteractiveSegment {
}

/// Part of a snippet that is filled in programically (like variables or shell code expansions).
pub trait Programic: InteractiveSegment {
	/// Set or reset the field used to convert implementor into string based upon evaluation of one implementor field.
	fn evaluate(&mut self);
	/// Return the a reference to the string field used to calculate/update another string field within a Programic implementor - the latter field being used to convert implementor into string.
	/// Used by PartialEq, Eq, PartialOrd and Ord implementations for dyn Programic.
	/// Basically used to enable ordering and testing of equallity for Programic trait objects.
	fn indentifier(&self) -> &String;
}
impl PartialEq for dyn Programic {
	fn eq(&self, other: &Self) -> bool {
		self.get_type().eq(other.get_type()) && self.indentifier().eq(other.indentifier())
	}
}
impl Eq for dyn Programic {}
impl PartialOrd for dyn Programic {
	fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
		Some(self.cmp(other))
	}
}
impl Ord for dyn Programic {
	fn cmp(&self, other: &Self) -> std::cmp::Ordering {
		let type_cmp = self.get_type().cmp(other.get_type());
		if type_cmp != std::cmp::Ordering::Equal {
			return type_cmp
		}
		self.indentifier().cmp(other.indentifier())
	}
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

/// Used to cast a type implementing Programic into into a concrete type implementing Programic.
/// This casting will not be mutable.
/// Can be used by things like [filter](std::iter::Iterator::filter) and especially [filter_map](std::iter::Iterator::filter_map).
pub fn cast_programic<P: Programic>(programic: &dyn Programic) -> Option<&P> {
	let interactive: &dyn InteractiveSegment = unsafe {
		std::mem::transmute(programic)
	};
	cast_interactive_segment::<P>(interactive)
}

/// Used to cast a type implementing Programic into into a concrete type implementing Programic.
/// This casting will be mutable.
/// Can be used by things like [filter](std::iter::Iterator::filter) and especially [filter_map](std::iter::Iterator::filter_map).
pub fn cast_mut_programic<P: Programic>(programic: &mut dyn Programic) -> Option<&mut P> {
	let interactive: &mut dyn InteractiveSegment = unsafe {
		std::mem::transmute(programic)
	};
	cast_mut_interactive_segment::<P>(interactive)
}

/// Selections within snippet to be cycled through and input by user.
pub struct Tab {
	/// Order by which this selection is cycled through.
	/// Example would be num = 1.
	/// This means it is the selection that will selected first.
	/// Num = 3 would mean it would be the selection that will be selected on the third cycle (usually by pressing tab in a text editor).
	pub num: u8,
	/// Selection to be input/changed by user.
	pub field: Rc<RefCell<dyn Field>>
}
impl fmt::Debug for Tab {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		f.debug_struct("Tab").field("num", &self.num).field("field", &(&*self.field.borrow())).finish()
	}
}
impl PartialEq for Tab {
	fn eq(&self, other: &Self) -> bool {
		self.num == other.num
	}
}
impl Eq for Tab {}
impl PartialOrd for Tab {
	fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
		Some(self.cmp(other))
	}
}
impl Ord for Tab {
	fn cmp(&self, other: &Self) -> std::cmp::Ordering {
		if self.num == 0 {
			return std::cmp::Ordering::Greater
		}
		if other.num == 0 {
			return std::cmp::Ordering::Less
		}
		self.num.cmp(&other.num)
	}
}

/// Snippet portion given unique name for reuse and clarity.
/// Good for reusing text in multiple places.
/// Or a InteractiveSegment that doesn't have a form of unique identification.
pub enum Reference {
	/// Reusable text.
	/// Use for lengthy text that is used in multiple places or for text you may want to change in multiple places in the future.
	Text(String, String),
	/// Reusable InteractiveSegment.
	/// Primarily use for a InteractiveSegment that doesn't have a unique identifier.
	/// Should not ever be used in [snippet body](Snippet::body).
	/// Instead equivalent Segment::Interactive should be used in body and Reference::InteractiveSegment should be found in the [snippet references](Snippet::references).
	Interactive(String, Rc<RefCell<dyn InteractiveSegment>>),
	/// Reusable group of snippet segments.
	/// Use when you have a group of segments occuring in multiple places in the same order together in each place.
	Group(String, Vec<Segment>)
}
impl fmt::Debug for Reference {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			Reference::Text(name, text) => f.debug_tuple("Reference::Text").field(name).field(text).finish(),
			Reference::Interactive(name, seg) => f.debug_tuple("Reference::Interactive").field(name).field(&(&*seg.borrow())).finish(),
			Reference::Group(name, group) => f.debug_tuple("Reference::Group").field(name).field(group).finish(),
		}
	}
}
impl fmt::Display for Reference {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			Reference::Text(_, value) => value.fmt(f),
			Reference::Interactive(_, interactive_value) => interactive_value.borrow().fmt(f),
			Reference::Group(_, body) => {
				for seg in body {
					seg.fmt(f)?;
				}
				Ok(())
			},
		}
	}
}
impl Reference {
	/// Get the name (unique identifier) of this reference.
	pub fn name(&self) -> String {
		match self {
			Reference::Text(name, _) => name,
			Reference::Interactive(name, _) => name,
			Reference::Group(name, _) => name,
		}.to_string()
	}
}
impl PartialEq for Reference {
	fn eq(&self, other: &Self) -> bool {
		self.name() == other.name()
	}
}
impl PartialEq<Segment> for Reference {
	fn eq(&self, other: &Segment) -> bool {
		if let Segment::Reference(other_reference) = other {
			return std::ptr::eq(self, &*other_reference.borrow())
		}
		let interactive = if let Reference::Interactive(_, interactive) = self {
			interactive
		} else {
			return false
		};
		let other_interactive = if let Segment::Interactive(interactive) = other {
			interactive
		} else {
			return false
		};
		Rc::ptr_eq(interactive, other_interactive)
	}
}
impl Eq for Reference {}
impl PartialOrd for Reference {
	fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
		Some(self.cmp(other))
	}
}
impl Ord for Reference {
	fn cmp(&self, other: &Self) -> std::cmp::Ordering {
		self.name().cmp(&other.name())
	}
}

/// Single portion of a snippet.
pub enum Segment {
	/// Just normal text.
	Text(String),
	/// Text filled in by user or program.
	Interactive(Rc<RefCell<dyn InteractiveSegment>>),
	/// Snippet portion given unique name for reuse and clarity.
	/// Good for reusing text in multiple places.
	/// Or a InteractiveSegment that doesn't have a form of unique identification.
	Reference(Rc<RefCell<Reference>>)
}
impl fmt::Debug for Segment {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			Segment::Text(text) => f.debug_tuple("Segment::Text").field(text).finish(),
			Segment::Interactive(seg) => f.debug_tuple("Segment::Interactive").field(&(&*seg.borrow())).finish(),
			Segment::Reference(reference) => f.debug_tuple("Segment::Reference").field(&(&*reference.borrow())).finish(),
		}
	}
}
impl fmt::Display for Segment {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			Segment::Text(text) => text.fmt(f),
			Segment::Interactive(seg) => seg.borrow().fmt(f),
			Segment::Reference(reference) => reference.borrow().fmt(f),
		}
	}
}
impl PartialEq for Segment {
	fn eq(&self, other: &Self) -> bool {
		if let Segment::Reference(reference) = self {
			return reference.borrow().eq(other)
		}
		if let Segment::Reference(other_reference) = other {
			return other_reference.borrow().eq(self)
		}
		if let Segment::Interactive(interactive) = self {
			if let Segment::Interactive(other_interactive) = other {
				return Rc::ptr_eq(interactive, other_interactive)
			}
		}
		false
	}
}
impl PartialEq<Reference> for Segment {
	fn eq(&self, other: &Reference) -> bool {
		other.eq(self)
	}
}

struct ProgramFilledTextDebugHelper(Rc<RefCell<dyn Programic>>);
struct ReferencesDebugHelper(Rc<RefCell<Reference>>);
impl fmt::Debug for ProgramFilledTextDebugHelper {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		fmt::Debug::fmt(&(&*self.0.borrow()), f)
	}
}
impl fmt::Debug for ReferencesDebugHelper {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		fmt::Debug::fmt(&*self.0.borrow(), f)
	}
}
/// Represents a snippet like found in atom, visual studio code or textmate.
pub struct Snippet {
	/// Actual content of the snippet.
	pub body: Vec<Segment>,
	/// List of each user filled in text.
	pub tabs: Vec<Tab>,
	/// List of each programically evaluated text.
	pub program_filled_text: Vec<Rc<RefCell<dyn Programic>>>,
	/// List of each reusable by name chunk(s) of the snippet.
	pub references: Vec<Rc<RefCell<Reference>>>
}
impl fmt::Debug for Snippet {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		let p: Vec<ProgramFilledTextDebugHelper> = (&self.program_filled_text).iter().map(|p: &Rc<RefCell<dyn Programic>>| {
			ProgramFilledTextDebugHelper(Rc::clone(p))
		}).collect();
		let r: Vec<ReferencesDebugHelper> = (&self.references).iter().map(|r: &Rc<RefCell<Reference>>|{
			ReferencesDebugHelper(Rc::clone(r))
		}).collect();
		f.debug_struct("Snippet")
		.field("body", &self.body)
		.field("tabs", &self.tabs)
		.field("program_filled_text", &p)
		.field("references", &r)
		.finish()
	}
}
impl fmt::Display for Snippet {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		for seg in &self.body {
			seg.fmt(f)?;
		}
		Ok(())
	}
}
impl InteractiveSegment for Snippet {
	fn get_type(&self) -> &str {
		"snippet"
	}
}
impl Snippet {
	/// Remove elements from snippet's tabs vec field whose field field are not found in snippet body.
	pub fn trim_empty_tabs(&mut self) {
		self.tabs.retain(|element: &Tab| Rc::strong_count(&element.field) > 1)
	}
	/// Remove elements from snippet's program_filled_text vec field who are not found in snippet body.
	pub fn trim_empty_program_filled_text(&mut self) {
		self.program_filled_text.retain(|element: &Rc<_>| Rc::strong_count(element) > 1)
	}
	/// Remove elements from snippet's references vec field who are not found in snippet body.
	pub fn trim_empty_references(&mut self) {
		self.references.retain(|element: &Rc<_>| Rc::strong_count(element) > 1)
	}
	/// Insert into snippet's tabs vec field in sorted order if last arguement is not already found in field (Eq trait of last arguement on each of the vec field's items).
	/// first part of tuple returned is whether insertion was done or not.
	/// Second part ot tuple returned is which index the last arguement was found/inserted at.
	pub fn insert_tab(&mut self, tab: Tab) -> (bool, usize) {
		match self.tabs.binary_search(&tab) {
			Ok(position) => (false, position),
			Err(position) => {
				self.tabs.insert(position, tab);
				(true, position)
			}
		}
	}
	/// Insert into snippet's program_filled_text vec field in sorted order if last arguement is not already found in field (Eq trait of last arguement on each of the vec field's items).
	/// first part of tuple returned is whether insertion was done or not.
	/// Second part ot tuple returned is which index the last arguement was found/inserted at.
	pub fn insert_program_filled_text(&mut self, programic: Rc<RefCell<dyn Programic>>) -> (bool, usize) {
		match self.program_filled_text.binary_search(&programic) {
			Ok(position) => (false, position),
			Err(position) => {
				self.program_filled_text.insert(position, programic);
				(true, position)
			}
		}
	}
	/// Insert into snippet's references vec field in sorted order if last arguement is not already found in field (Eq trait of last arguement on each of the vec field's items).
	/// first part of tuple returned is whether insertion was done or not.
	/// Second part ot tuple returned is which index the last arguement was found/inserted at.
	pub fn insert_reference(&mut self, reference: Rc<RefCell<Reference>>) -> (bool, usize) {
		match self.references.binary_search(&reference) {
			Ok(position) => (false, position),
			Err(position) => {
				self.references.insert(position, reference);
				(true, position)
			}
		}
	}
}

