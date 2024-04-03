with Ada.IO_Exceptions;
with Ada.Text_IO;			use Ada.Text_IO;
with Ada.Integer_Text_IO;	use Ada.Integer_Text_IO;
with Ada.Command_line;		use Ada.Command_line;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Matrix;
with Sparse_Matrix;
with Vector;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Generic_Array_Sort;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Vectors;

procedure PageRank is
    -- Command line setup
    No_Argument_Error: Exception;
    Argument_Missing_Error: Exception;
    Unknown_Argument_Error: Exception;
    Out_Of_Bounds_Error: Exception;
    File: Ada.Text_IO.File_Type;
    Arg_Counter: Positive := 1;

    procedure Display_Usage is
    begin
        Put_Line ("Usage : " & Command_Name & " [-A <alpha>] [-K <max_iter>] [-E <epsilon>] [-P|-C] [-R <prefix>] <reseau>");
        Put_Line ("Options :");
        Put_Line ("  -A <alpha>    : Fixe le paramètre alpha à <alpha>, valeur comprise entre 0 et 1 inclus (par défaut : 0.85)");
        Put_Line ("  -K <max_iter> : Fixe le nombre maximum d'itérations, strictement positif, de vecteurs de poids à calculer (par défaut : 150)");
        Put_Line ("  -E <epsilon>  : Fixe à <epsilon>, positif, la variation minimale de distance entre deux vecteurs de poids consécutifs avant l'arrêt du programme (par défaut : 0.0)");
        Put_Line ("  -P            : Utilise la version avec matrices pleines de l'algorithme");
        Put_Line ("  -C            : Utilise la version avec matrices creuses de l'algorithme (par défaut)");
        Put_Line ("  -R <prefix>   : Préfixe les fichiers de sortie avec <prefix> (par défaut 'output')");
        Put_Line ("  <reseau>      : Fichier .net représentant le graphe à analyser");
    end Display_Usage;

    function Last_Param(Arg_Count: Integer) return String is
    begin
        return Argument(Arg_Count)(2..Argument(Arg_Count)'Length);
    end Last_Param;

    -- Utility setup
    subtype T_Element is Long_Float; -- Can be changed to any type of floating point number
    package Element_IO is new Ada.Text_IO.Float_IO (T_Element);
    use Element_IO; -- Enables the use of Put and Get for T_Element
    INIT_SUCCESS: Boolean := False;
    garbage: Integer;
    NNZ: Integer := 0;

    -- PageRank algorithm parameters
    ALPHA: T_Element := 0.85;
    MAX_ITER: Integer := 150;
    EPSILON: T_Element := 0.0;
    PREFIX: Unbounded_String := To_Unbounded_String("output");
    type MODES is (Plein, Creux);
    MODE: MODES := Creux;
    N: Integer;

begin
    -- Setting float display parameters
    Element_IO.Default_Exp := 0;

    -- Command line parsing loop
    begin
        if Argument_Count < 1 then
            raise No_Argument_Error;
        end if;
        while Arg_Counter < Argument_Count + 1 loop
            if Argument(Arg_Counter) = "-A" then
                Arg_Counter := Arg_Counter + 1;
                if Arg_Counter >= Argument_Count then
                    raise Argument_Missing_Error;
                end if;
                ALPHA := T_Element'Value(Argument(Arg_Counter));
                if ALPHA > 1.0 or else ALPHA < 0.0 then
                    raise Out_Of_Bounds_Error;
                end if;
            elsif Argument(Arg_Counter) = "-K" then
                Arg_Counter := Arg_Counter + 1;
                if Arg_Counter >= Argument_Count then
                    raise Argument_Missing_Error;
                end if;
                MAX_ITER := Integer'Value(Argument(Arg_Counter));
                if MAX_ITER <= 0 then
                    raise Out_Of_Bounds_Error;
                end if;
            elsif Argument(Arg_Counter) = "-E" then
                Arg_Counter := Arg_Counter + 1;
                if Arg_Counter >= Argument_Count then
                    raise Argument_Missing_Error;
                end if;
                EPSILON := T_Element'Value(Argument(Arg_Counter));
                if EPSILON < 0.0 then
                    raise Out_Of_Bounds_Error;
                end if;
            elsif Argument(Arg_Counter) = "-P" then
                MODE := Plein;
            elsif Argument(Arg_Counter) = "-C" then
                MODE := Creux;
            elsif Argument(Arg_Counter) = "-R" then
                Arg_Counter := Arg_Counter + 1;
                if Arg_Counter >= Argument_Count then
                    raise Argument_Missing_Error;
                end if;
                PREFIX := To_Unbounded_String(Argument(Arg_Counter));
            else
                if Arg_Counter = Argument_Count then
                    Open (File, In_File, Argument(Argument_Count));
                    Get (File, N);
                    if MODE = Creux then
                        while not End_Of_file (File) loop
                            Get (File, garbage);
                            Get (File, garbage);
                            NNZ := NNZ + 1;
                        end loop;
                    end if;
                    Close (File);
                    INIT_SUCCESS := True;
                else
                    raise Unknown_Argument_Error;
                end if;
            end if;
            Arg_Counter := Arg_Counter + 1;
        end loop;
    exception
        when Constraint_Error =>
            Put_Line ("Mauvais type de l'argument : " & Last_Param(Arg_Counter - 1));
            New_Line;
            Display_Usage;
        when Argument_Missing_Error =>
            Put_Line ("Argument manquant pour : " & Last_Param(Arg_Counter - 1));
            New_Line;
            Display_Usage;
        when No_Argument_Error =>
            Put_Line ("Pas de fichier spécifié.");
            New_Line;
            Display_Usage;
        when Ada.IO_Exceptions.Name_Error =>
            Put_Line ("Fichier inexistant : " & Argument(Argument_Count));
        when Unknown_Argument_Error =>
            if Argument(Arg_Counter)(1) = '-' then
                Put_Line ("Paramètre inconnu : " & Last_Param(Arg_Counter));
            else
                Put_Line ("Argument inconnu : " & Argument(Arg_Counter));
            end if;
            New_Line;
            Display_Usage;
        when Data_Error =>
            Put_Line ("Mauvais type d'une valeur du fichier d'entrée.");
        when End_Error =>
            Put_Line ("Mauvais format du fichier d'entrée.");
    end;
    
    if not INIT_SUCCESS then
        return;
    end if;

    -- PageRank algorithm
    if MODE = Plein then
        declare
            package New_Vector is new Vector(N, T_Element); use New_Vector;
            PI: T_Vector;
            PIprec: T_Vector;
            Result: T_Indexed_Vector;

            package New_Matrix is new Matrix (N, T_Element, New_Vector); use New_Matrix;
            G: T_Matrix;
            
            -- Intermediary variables
            S: T_Matrix;
            J: T_Matrix;
            a: Integer;
            b: Integer;
            Nb_Iter: Integer := 1;
            
            procedure Line_Init (I : in Integer) is
                Temp: T_Element := 0.0;
            begin
                for J in 1..N loop
                    Temp := Temp + S(I, J);
                end loop;
                if Temp = 0.0 then
                    for J in 1..N loop
                        S(I, J) := 1.0 / T_Element(N);
                    end loop;
                else
                    for J in 1..N loop
                        if S(I, J) /= 0.0 then
                            S(I, J) := S(I, J) / Temp;
                        end if;
                    end loop;
                end if;
            end Line_Init;
        begin
            -- Setup
            Ones(S);
            Ones(J);
            S := 0.0 * S;
            J := (1.0 / T_Element(N)) * J;

            for I in 1..N loop
                PI(I) := 1.0 / T_Element(N);
            end loop;

            -- Computing the S matrix
            begin
                Open (File, In_File, Argument(Argument_Count));
                Get (File, N);
                while not End_Of_file (File) loop
                    Get (File, a);
                    Get (File, b);
                    S(a + 1, b + 1) := 1.0;
                end loop;
                Close (File);
            exception
                when Data_Error =>
                    Put_Line ("Mauvais type d'une valeur du fichier d'entrée.");
                when End_Error =>
                    Put_Line ("Mauvais format du fichier d'entrée.");
            end;
            for I in 1..N loop
                Line_Init (I);
            end loop;

            -- Computing the G matrix
            G := ALPHA*S + (1.0 - ALPHA)*J;

            -- Main algorithm iteration
            PIprec := PI;
            PI := Vector_Multiply(PI, G);
            while (Nb_Iter < MAX_ITER) and then (Vector_Distance(PI, PIprec, 1.0) > EPSILON) loop
                PIprec := PI;
                PI := Vector_Multiply(PI, G);
                Nb_Iter := Nb_Iter + 1;
            end loop;

            -- Sorting the final result
            Result := Vector_Sort(PI);

            -- Saving the final output files
            -- PageRank file (.pr extension)
            Create (File, Out_File, To_String (PREFIX) & ".pr");
            for I in 1..N loop
                Put (File, Result(I).Index-1, 0);
                New_Line (File);
            end loop;
            Close (File);

            -- Node weights (.prw extension)
            Create (File, Out_File, To_String (PREFIX) & ".prw");
            Put (File, N, 0);
            Put (File, " ");
            Put (File, ALPHA, 0);
            Put (File, " ");
            Put (File, MAX_ITER, 0);
            New_Line (File);
            for I in 1..N loop
                Put (File, Result(I).Value, 0);
                New_Line (File);
            end loop;
            Close (File);
        end;
    else
        declare
            package New_Vector is new Vector(N, T_Element); use New_Vector;
            PI: T_Vector;
            PIprec: T_Vector;
            Result: T_Indexed_Vector;

            package Integer_Vectors is new Ada.Containers.Vectors (Index_Type => Positive, Element_Type => Integer);
            use Integer_Vectors;
            Coefficients: Integer_Vectors.Vector;

            package New_Matrix is new Sparse_Matrix(N, NNZ, T_Element, New_Vector, Integer_Vectors); use New_Matrix;
            G: T_Sparse_Matrix;

            -- Some utilities to help initialize the CSC from the directed edges list
            type Arc is record
                Left: Integer;
                Right: Integer;
            end record;
            type Arcs is array(Positive range <>) of Arc;
            Edges: Arcs(1..NNZ);

            function "<" (a, b: Arc) return Boolean is
            begin
                return a.Right < b.Right or (a.Right = b.Right and a.Left < b.Left);
            end "<";
            
            procedure Sort_Arcs is new Ada.Containers.Generic_Array_Sort
            (Index_Type     => Positive, 
            Element_Type    => Arc, 
            Array_Type      => Arcs,
            "<"             => "<");

            procedure Invert_Arcs(A: in out Arcs) is
                Temp: Arc;
            begin
                for I in 1..NNZ loop
                    Temp := A(I);
                    A(I).Left := Temp.Right;
                    A(I).Right := Temp.Left;
                end loop;
            end Invert_Arcs;

            -- Intermediary variables
            Arcs_I: Integer := 1;
            a: Integer;
            b: Integer;
            column_index: Integer := 1;
            line_index: Integer;
            line_size: Integer := 0;
            Nb_Iter: Integer := 1;

            -- A map to store the number of outgoing arcs for each node
            function Hash (Key : Integer) return Hash_Type is
            begin
                return Hash_Type(Key);
            end Hash;

            package Integer_Hash_Map is new Ada.Containers.Hashed_Maps
            (Key_Type       => Integer,
            Element_Type    => T_Element,
            Hash            => Hash,
            Equivalent_Keys => "=");
            use Integer_Hash_Map;

            Map: Integer_Hash_Map.Map;
        begin
            -- Setup
            for I in 1..N loop
                PI(I) := 1.0 / T_Element(N);
            end loop;

            -- First we get the ordered list of arcs
            begin
                Open (File, In_File, Argument(Argument_Count));
                Get (File, N);
                while not End_Of_file (File) loop
                    Get (File, a);
                    Get (File, b);
                    Edges(Arcs_I).Left := a + 1;
                    Edges(Arcs_I).Right := b + 1;
                    Arcs_I := Arcs_I + 1;
                end loop;
                Close (File);
            exception
                when Data_Error =>
                    Put_Line ("Mauvais type d'une valeur du fichier d'entrée.");
                when End_Error =>
                    Put_Line ("Mauvais format du fichier d'entrée.");
            end;
            Sort_Arcs(Edges); -- This is absolutely necessary, as the .net files are not necessarily sorted

            -- Computing the adjacency matrix
            Ones(G);
            for I in 1..NNZ loop
                if Edges(I).Right > column_index then
                    for J in (column_index+1)..Edges(I).Right loop
                        G.Column_Pointers(J) := I;
                    end loop;
                    column_index := Edges(I).Right;
                end if;
                G.Row_Indices(I) := Edges(I).Left;
            end loop;
            for J in (column_index+1)..N loop
                G.Column_Pointers(J) := G.Column_Pointers(column_index);
            end loop;
            G.Column_Pointers(N+1) := NNZ + 1;

            -- Computing the ALPHA*H matrix
            Invert_Arcs(Edges);
            Sort_Arcs(Edges);
            line_index := Edges(1).Right;
            for I in 1..NNZ loop
                if Edges(I).Right > line_index then
                    Map.Insert(Key => line_index, New_Item => T_Element(line_size));
                    line_index := Edges(I).Right;
                    line_size := 0;
                end if;
                line_size := line_size + 1;
            end loop;
            Map.Insert(Key => line_index, New_Item => T_Element(line_size));

            for I in 1..NNZ loop
                G.Values(I) := ALPHA * G.Values(I) / Map.Element(G.Row_Indices(I));
            end loop;

            -- Computing Coefficients, a vector of the indices of the nodes with no outgoing arcs
            for I in 1..N loop
                if not Integer_Hash_Map.Contains(Map, I) then
                    Coefficients.Append(I); -- If I is not in the Map, the node has no outgoing arcs
                end if;
            end loop;

            -- Main algorithm iteration
            PIprec := PI;
            PI := Next_Weight_Vector(PI, Coefficients, G, ALPHA);
            while (Nb_Iter < MAX_ITER) and then (Vector_Distance(PI, PIprec, 1.0) > EPSILON) loop
                PIprec := PI;
                PI := Next_Weight_Vector(PI, Coefficients, G, ALPHA);
                Nb_Iter := Nb_Iter + 1;
            end loop;

            -- Sorting the final result
            Result := Vector_Sort(PI);

            -- Saving the final output files
            -- PageRank file (.pr extension)
            Create (File, Out_File, To_String (PREFIX) & ".pr");
            for I in 1..N loop
                Put (File, Result(I).Index-1, 0);
                New_Line (File);
            end loop;
            Close (File);

            -- Node weights (.prw extension)
            Create (File, Out_File, To_String (PREFIX) & ".prw");
            Put (File, N, 0);
            Put (File, " ");
            Put (File, ALPHA, 0);
            Put (File, " ");
            Put (File, MAX_ITER, 0);
            New_Line (File);
            for I in 1..N loop
                Put (File, Result(I).Value, 0);
                New_Line (File);
            end loop;
            Close (File);
        end;
    end if;
end PageRank;
