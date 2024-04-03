package body Sparse_Matrix is
    procedure Ones(M: out T_Sparse_Matrix) is
    begin
        M.Values := (others => 1.0);
        M.Row_Indices := (others => 1);
        M.Column_Pointers := (others => 1);
    end Ones;

    function Next_Weight_Vector(Weight_Vector: in Vector_Instance.T_Vector; Coefficients: Integer_Vectors.Vector; G: in T_Sparse_Matrix; ALPHA: T_Element) return Vector_Instance.T_Vector is
        Result: Vector_Instance.T_Vector;
        Temp: T_Element;
        base_value: T_Element := (1.0 - ALPHA) / T_Element(N);
        alpha_over_n: T_Element := ALPHA / T_Element(N);
    begin
        for K of Coefficients loop
            base_value := base_value + Weight_Vector(K)*alpha_over_n;
        end loop;
        for I in 1..N loop
            Temp := base_value;
            for J in G.Column_Pointers(I)..G.Column_Pointers(I+1)-1 loop
                Temp := Temp + G.Values(J) * Weight_Vector(G.Row_Indices(J));
            end loop;
            Result(I) := Temp;
        end loop;
        return Result;
    end Next_Weight_Vector;
end Sparse_Matrix;
