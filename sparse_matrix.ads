-- Here we define a short Sparse_Matrix module to make the operations needed in the main program more readable
-- Arguments to instantiate the module:
--     N: size of the square matrix
--     NNZ: number of non-zero entries (IMPORTANT: this remains constant, although G has only strictly positive entries)
--     T_Element: type of its elements, some type of float
with Vector;
with Ada.Containers.Vectors;

generic
    N: Positive;
    NNZ: Positive;
    type T_Element is digits <>;
    with package Vector_Instance is new Vector(N, T_Element);
    with package Integer_Vectors is new Ada.Containers.Vectors (Index_Type => Positive, Element_Type => Integer);
package Sparse_Matrix is
    -- We'll use a compressed sparse column (CSC) data structure in order to store the sparse matrix
    type T_Elements is array (1..NNZ) of T_Element;
    type T_Row_Indices is array (1..NNZ) of Positive;
    type T_Column_Indices is array (1..N+1) of Positive;

    type T_Sparse_Matrix is record
        Values: T_Elements;
        Row_Indices: T_Row_Indices;
        Column_Pointers: T_Column_Indices;
    end record;

    -- We'll only define the operations needed for our PageRank implementation
    -- This initializes a N x N matrix with ones in place of all non-zero entries
    procedure Ones(M: out T_Sparse_Matrix);

    -- This function computes the next iteration of the weight vector with the sparse matrix G
    function Next_Weight_Vector(Weight_Vector: in Vector_Instance.T_Vector; Coefficients: Integer_Vectors.Vector; G: in T_Sparse_Matrix; ALPHA: T_Element) return Vector_Instance.T_Vector;
end Sparse_Matrix;
