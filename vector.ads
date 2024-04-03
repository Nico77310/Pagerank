generic
    N: Positive;
    type T_Element is digits <>;
package Vector is
    type T_Vector is array (1..N) of T_Element;

    -- This function calculates the distance between two vectors using the specified p-norm
    function Vector_Distance(U: in T_Vector; V: in T_Vector; p: in T_Element) return T_Element with
        Pre => p >= 1.0,
        Post => Vector_Distance'Result >= 0.0;

    -- A vector sorting function which returns the indices of the original vector in sorted order,
    -- as well as the sorted vector
    type T_Cell is record
        Index: Positive;
        Value: T_Element;
    end record;
    type T_Indexed_Vector is array (1..N) of T_Cell;
    function Vector_Sort(V: in T_Vector) return T_Indexed_Vector;
end Vector;