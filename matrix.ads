-- Here we define a short Matrix module to make the operations needed in the main program more readable
-- Arguments to instantiate the module:
--     N: size of the square matrix
--     T_Element: type of its elements, some type of float
with Vector;

generic
    N: Positive;
    type T_Element is digits <>;
    with package Vector_Instance is new Vector(N, T_Element);
package Matrix is
    type T_Matrix is array (1..N, 1..N) of T_Element;

    -- We'll only define the operations needed for our PageRank implementation
    -- This initializes a N x N matrix with ones
    procedure Ones (M: out T_Matrix);
    
    -- Overloading the "+" operator to add two matrices
    function "+" (Left, Right : in T_Matrix) return T_Matrix;

    -- Overloading the "*" operator to multiply a scalar by a matrix
    function "*" (Scalar : in T_Element; Right : in T_Matrix) return T_Matrix;

    -- This function multiplies a line vector of size N by a matrix of size N x N
    function Vector_Multiply (Left: in Vector_Instance.T_Vector; Right: in T_Matrix) return Vector_Instance.T_Vector;
    
    -- Utility function to print a matrix
    generic
        with procedure Put_Element (Item : in T_Element);
    procedure Matrix_Print (M: in T_Matrix);
end Matrix;