with Ada.Text_IO; use Ada.Text_IO;

package body Matrix is
    function "+" (Left, Right: in T_Matrix) return T_Matrix is
        Result: T_Matrix;
    begin
        for I in 1..N loop
            for J in 1..N loop
                Result(I, J) := Left(I, J) + Right(I, J);
            end loop;
        end loop;
        return Result;
    end "+";


    function "*" (Scalar: in T_Element; Right: in T_Matrix) return T_Matrix is
        Result: T_Matrix;
    begin
        for I in 1..N loop
            for J in 1..N loop
                Result(I, J) := Scalar * Right(I, J);
            end loop;
        end loop;
        return Result;
    end "*";


    function Vector_Multiply (Left: in Vector_Instance.T_Vector; Right: in T_Matrix) return Vector_Instance.T_Vector is
        Result: Vector_Instance.T_Vector;
        Temp: T_Element;
    begin
        for I in 1..N loop
            Temp := 0.0;
            for J in 1..N loop
                Temp := Temp + Left(J) * Right(J, I);
            end loop;
            Result(I) := Temp;
        end loop;
        return Result;
    end Vector_Multiply;


    procedure Ones(M: out T_Matrix) is
    begin
        for I in 1..N loop
            for J in 1..N loop
                M(I, J) := 1.0;
            end loop;
        end loop;
    end Ones;


    procedure Matrix_Print(M: in T_Matrix) is
    begin
        for I in 1..N loop
            for J in 1..N loop
                Put_Element(M(I, J));
                Put(" ");
            end loop;
            New_Line;
        end loop;
    end Matrix_Print;
end Matrix;