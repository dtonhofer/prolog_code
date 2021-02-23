'?' signifies that the term is currently "texty" but that may change depending on future instantiations.
    In some cases, instantiations can move a term "downwards" in the tree, for example a
    var may become an openvarlist which may become a varlist which which may become a codelist.   
'!' signifies that the term is currently "texty" and that will stay so for sure.
'*' signifies that subcategories exist which have '?' and others which have '!'

Categorization always categorizes a term as not belonging to this tree or else as 
belonging to exactly one of the leaf categories. Higher categories just regroup
sub categories in a meaningful way (for the programmer).

           +--- var? (could be anything)
           |
           |
           |                   +--- openvarlist?: nonempty open list of >= 1 var
           |                   |
           +--- openlist? -----+--- opencharylist?: nonempty open list of >= 1 char, may contain vars
           |                   |
           |                   +--- opencodeylist?: nonempty open list of >= 1 unicode code points, may contain vars
           |
           |                   +--- varlist? list of N>=1 vars; edge case
           |                   |
           +--- changylist? ---+--- charylist? list of C>=1 chars and V>=1 vars
           |                   |
           |                   +--- codeylist? list of C>=1 unicode code points and V>=1 vars
           |
           |                                               +--- emptylist! empty list; edge case, not sure whether charlist or codelist, there is not enough info
           |                                               |           
           |                             +--- textlist! ---+--- charlist! nonempty list of chars ("characters", atoms of length 1)
           |                             |                 |
           |                             |                 +--- codelist! nonempty list of unicode code points
           |                             |                               
 texty* ---+               +--- text! ---+
           |               |             |
           |               |             |                +--- atom! (including the empty atom)
           |               |             |                |
           +---anytext! ---+             +--- stringy! ---+
                           |                              |
                           |                              +--- string! (SWI-Prolog strings including the empty string)
                           |
                           +--- number!  acceptable because a number can be
                                         transformed into text (according to some
                                         unspecified convention...)
          
           
success
 settled : it is texty and will stay texty (actually, it is in the category and will stay in the category)
 free:     it is text now but may become nontexty depending on future instantiations
failure
 it's not texty

attributes

 empty     if it is a closed list, it is empty, if it is an atom, it is the empty atom, if it is a string, it is the empty string
 length(N) if it is a list, it has length N (even for an open list)
 vars(N)   if it is a list, it has N vars
 open      if it is a list, it's an open list
 closed    if it is a list, it's a closed list

category

%! textycat(@X,?Cat,?Surety,?Attributes)

textycat(X,C) :- nonvar(C), textycat(X,CC),isa(CC,C).

textycat(X,var)    :- var(X),!.
textycat(X,string) :- string(X),!.
textycat(X,atom)   :- atom(X),!.

validcat(C) :-


isa(SubCat,SuperCat) :- parent(SubCat,SuperCat).
isa(SubCat,SuperCat) :- parent(SubCat,NextCat),isa(NextCat,SuperCat).

parent(var,texty).
parent(anytext,texty).
parent(openlist,texty).
parent(number,anytext).
parent(text,anytext).
parent(list,text).
parent(stringy,text).
parent(atom,stringy).
parent(string,stringy).
parent(charlist,list).
parent(codelist,list).
parent(varlist,list).
parent(emptylist,list).
parent(openvarlist,openlist).
parent(opencharlist,openlist).
parent(opencodelist,openlist).

canbe(var).
canbe(varlist).
canbe(emptylist).
canbe(openlist).


valid_cat().


parent(var,texty).
parent(anytext,texty).
parent(openlist,texty).
parent(number,anytext).
parent(text,anytext).
parent(list,text).
parent(stringy,text).
parent(atom,stringy).
parent(string,stringy).
parent(charlist,list).
parent(codelist,list).
parent(varlist,list).
parent(emptylist,list).
parent(openvarlist,openlist).
parent(opencharlist,openlist).
parent(opencodelist,openlist).

