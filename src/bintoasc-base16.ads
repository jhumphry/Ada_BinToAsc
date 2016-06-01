-- BinToAsc.Base16
-- Binary data to ASCII codecs - Base16 codec as in RFC4648

-- Copyright (c) 2015, James Humphry - see LICENSE file for details

generic
   Alphabet : Alphabet_16;
   Case_Sensitive : Boolean;
package BinToAsc.Base16
with SPARK_Mode => On is

   type Base16_To_String is new Codec_To_String with null record;

   overriding
   procedure Reset (C : out Base16_To_String);

   pragma Warnings (GNATprove, Off, "unused variable ""C""");
   overriding
   function Input_Group_Size (C : in Base16_To_String) return Positive is (1);

   overriding
   function Output_Group_Size (C : in Base16_To_String) return Positive is (2);
   pragma Warnings (GNATprove, On, "unused variable ""C""");

   overriding
   procedure Process (C : in out Base16_To_String;
                      Input : in Bin;
                      Output : out String;
                      Output_Length : out Natural);

   overriding
   procedure Process (C : in out Base16_To_String;
                      Input : in Bin_Array;
                      Output : out String;
                      Output_Length : out Natural)
     with Post => (C.State = Ready and
                     Output'Length / Output_Group_Size(C) >=
                       Input'Length / Input_Group_Size(C) + 1 and
                       Output_Length mod 2 = 0);

   overriding
   procedure Complete (C : in out Base16_To_String;
                        Output : out String;
                        Output_Length : out Natural)
     with Post => (C.State in Completed | Failed and
                     Output_Length in 0 | 2);

   function To_String (Input : in Bin_Array) return String;

   type Base16_To_Bin is new Codec_To_Bin with private;

   overriding
   procedure Reset (C : out Base16_To_Bin);

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

   function To_Bin (Input : in String) return Bin_Array;

private

   subtype Half_Bin is Bin range 0..15;

   type Base16_To_Bin is new Codec_To_Bin with
      record
         Loaded : Boolean := False;
         Load : Half_Bin := 0;
      end record;

end BinToAsc.Base16;
