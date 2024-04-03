with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Containers.Generic_Array_Sort;
with Ada.Text_IO; use Ada.Text_IO;

package body Vector is
    -- This module defines the ** and Abs functions for the specified float type
    package Elementary_Functions is 
        new Ada.Numerics.Generic_Elementary_Functions (T_Element);
    use Elementary_Functions;

    function Vector_Distance(U: in T_Vector; V: in T_Vector; p: in T_Element) return T_Element is
        Result: T_Element := 0.0;
    begin
        for I in 1..N loop
            Result := Result + (Abs(U(I)-V(I))) ** p;
        end loop;
        return Result **  (1.0 / p);
    end Vector_Distance;
    
    --  VERSION 1
    --  function Vector_Sort(V: in T_Vector) return T_Indexed_Vector is
    --  Result: T_Indexed_Vector;
    --  Temp: T_Cell;
    --  Max_Index: Positive;
    --  begin
    --      -- Initializing the Indexed_Vector
    --      for I in 1..N loop
    --          Result(I) := (I, V(I));
    --      end loop;

    --      -- Using the selection sort algorithm
    --      for I in 1..(N - 1) loop
    --          Max_Index := I;
    --          for J in (I + 1)..N loop
    --              if Result(Max_Index).Value < Result(J).Value then
    --                  Max_Index := J;
    --              end if;
    --          end loop;
    --          -- Swapping the max valued and original cells
    --          Temp := Result(Max_Index);
    --          Result(Max_Index) := Result(I);
    --          Result(I) := Temp;
    --      end loop;
    --      return Result;        
    --  end Vector_Sort;
    
    --  VERSION 2 using the Ada library for sorting algorithms
    function Cell_Compare(a, b: T_Cell) return Boolean is
    begin
        return a.Value > b.Value;
    end Cell_Compare;
    
    type T_IndVector_Unconstrained is array (Positive range <>) of T_Cell;
    procedure Sort_Ind_Vect is new Ada.Containers.Generic_Array_Sort
    (Index_Type     => Positive, 
    Element_Type    => T_Cell, 
    Array_Type      => T_IndVector_Unconstrained,
    "<"             => Cell_Compare);

    function Vector_Sort(V: in T_Vector) return T_Indexed_Vector is
    Result_temp: T_IndVector_Unconstrained(1..N);
    Result: T_Indexed_Vector;
    Temp: T_Cell;
    Max_Index: Positive;
    begin
        for I in 1..N loop
            Result_temp(I) := (I, V(I));
        end loop;
        Sort_Ind_Vect (Result_temp);
        for I in 1..N loop
            Result(I) := Result_temp(I);
        end loop;    
        return Result;
    end Vector_Sort;
end Vector;