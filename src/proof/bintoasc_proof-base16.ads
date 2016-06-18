-- BinToAsc_Proof.Base16
-- Binary data to ASCII codecs - Base16 codec as in RFC4648

-- Copyright (c) 2015, James Humphry - see LICENSE file for details

package BinToAsc_Proof.Base16
with SPARK_Mode => On is

   -- Static definitions
   Alphabet : constant BinToAsc_Proof.Alphabet_16
     := "0123456789ABCDEF";
   Case_Sensitive : constant Boolean := True;

   type Base16_To_String is new Codec_To_String with null record;

   pragma Warnings (GNATprove, Off, "unused variable ""C""");
   overriding function Empty (C : in Base16_To_String) return Boolean;
   pragma Warnings (GNATprove, On, "unused variable ""C""");

   overriding
   procedure Reset (C : out Base16_To_String)
     with Post => (C.State = Ready and Empty(C));

   pragma Warnings (GNATprove, Off, "unused variable ""C""");
   overriding
   function Input_Group_Size (C : in Base16_To_String) return Positive is (1);

   overriding
   function Output_Group_Size (C : in Base16_To_String) return Positive is (2);

   pragma Warnings (GNATprove, Off, """C"" is not modified, could be IN");
   overriding
   procedure Process (C : in out Base16_To_String;
                      Input : in Bin;
                      Output : out String;
                      Output_Length : out Natural);
   pragma Warnings (GNATprove, On, "unused variable ""C""");

   overriding
   procedure Process (C : in out Base16_To_String;
                      Input : in Bin_Array;
                      Output : out String;
                      Output_Length : out Natural)
     with Post => (C.State in Ready | Failed and
                     (
                      (Empty(C'Old) and
                        Output_Length / Output_Group_Size(C) <=
                        Input'Length / Input_Group_Size(C)
                     ) or
                        Output_Length / Output_Group_Size(C) <=
                      (Input'Length + Input_Group_Size(C) - 1) / Input_Group_Size(C)
                     ) and
                       Output_Length mod 2 = 0
                  );
    pragma Warnings (GNATprove, On, """C"" is not modified, could be IN");

   pragma Warnings (GNATprove, Off, "unused initial value of ""C""");
   overriding
   procedure Complete (C : in out Base16_To_String;
                        Output : out String;
                        Output_Length : out Natural)
     with Post => (C.State in Completed | Failed and
                     Output_Length in 0 | 2);
   pragma Warnings (GNATprove, On, "unused initial value of ""C""");

   function To_String (Input : in Bin_Array) return String
     with Pre => (Input'Length < ((Integer'Last/2 -  1) * 1));

   type Base16_To_Bin is new Codec_To_Bin with private;

   overriding function Empty (C : in Base16_To_Bin) return Boolean;

   overriding
   procedure Reset (C : out Base16_To_Bin)
     with Post => (C.State = Ready and Empty(C));

   pragma Warnings (GNATprove, Off, "unused variable ""C""");
   overriding
   function Input_Group_Size (C : in Base16_To_Bin) return Positive is (2);

   overriding
   function Output_Group_Size (C : in Base16_To_Bin) return Positive is (1);
   pragma Warnings (GNATprove, On, "unused variable ""C""");

   overriding
   procedure Process (C : in out Base16_To_Bin;
                      Input : in Character;
                      Output : out Bin_Array;
                      Output_Length : out Bin_Array_Index);

   overriding
   procedure Process (C : in out Base16_To_Bin;
                      Input : in String;
                      Output : out Bin_Array;
                      Output_Length : out Bin_Array_Index);

   overriding
   procedure Complete (C : in out Base16_To_Bin;
                        Output : out Bin_Array;
                        Output_Length : out Bin_Array_Index)
     with Post => (C.State in Completed | Failed and
                     Output_Length = 0);

   function To_Bin (Input : in String) return Bin_Array
     with Pre => (Input'Length / 2  < (Bin_Array_Index'Last/1 -  1) and
                      Input'Last < Integer'Last);

private

   overriding function Empty (C : in Base16_To_String) return Boolean is (True);

   subtype Half_Bin is Bin range 0..15;

   type Base16_To_Bin is new Codec_To_Bin with
      record
         Loaded : Boolean := False;
         Load : Half_Bin := 0;
      end record;

   overriding function Empty (C : in Base16_To_Bin) return Boolean
   is (not C.Loaded);

end BinToAsc_Proof.Base16;
