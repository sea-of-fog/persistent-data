type 'a tree =
    | Leaf
    | Node of 'a tree * 'a * 'a tree

type 'a digits = int * 'a tree

type 'a slist = {
    lzeroes : int;
    two : 'a tree * 'a tree;
    rest : 'a digits
}
