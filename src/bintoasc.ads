-- BinToAsc
-- Binary data to ASCII codecs

-- Copyright (c) 2015, James Humphry
--
--  Permission to use, copy, modify, and/or distribute this software for any
--  purpose with or without fee is hereby granted, provided that the above
--  copyright notice and this permission notice appear in all copies.
--
--  THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
--  WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
--  MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
--  ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
--  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION
--  OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
--  CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

generic
   type Bin is mod <>;
   type Bin_Array_Index is range <>;
   type Bin_Array is array (Bin_Array_Index range <>) of Bin;
package BinToAsc is

   type Codec_State is (Ready, Completed, Failed);

   type Codec is abstract tagged record
      State : Codec_State := Ready;
   end record;

   procedure Reset (C : out Codec) is abstract
     with Post'Class => (C.State = Ready);
   -- Reset a Codec to its initial state

   function Input_Group_Size (C : in Codec) return Positive is abstract;

   function Output_Group_Size (C : in Codec) return Positive is abstract;

   type Codec_To_String is abstract new Codec with null record;

   procedure Process (C : in out Codec_To_String;
                      Input : in Bin;
                      Output : out String;
                      Output_Length : out Natural) is abstract
     with Pre'Class => (C.State = Ready and
                          Output'Length >= Output_Group_Size(C));

   procedure Process (C : in out Codec_To_String;
                      Input : in Bin_Array;
                      Output : out String;
                      Output_Length : out Natural) is abstract
     with Pre'Class => (C.State = Ready and
                          Output'Length / Output_Group_Size(C) >=
                            Input'Length / Input_Group_Size(C) + 1);

   procedure Complete (C : in out Codec_To_String;
                        Output : out String;
                        Output_Length : out Natural) is abstract
     with Pre'Class => (C.State = Ready and
                          Output'Length >= Output_Group_Size(C)),
       Post'Class => C.State in Completed | Failed;

   type Codec_To_Bin is abstract new Codec with null record;

   procedure Process (C : in out Codec_To_Bin;
                      Input : in Character;
                      Output : out Bin_Array;
                      Output_Length : out Bin_Array_Index) is abstract
     with Pre'Class => (C.State = Ready and
                          Output'Length >= Output_Group_Size(C));

   procedure Process (C : in out Codec_To_Bin;
                      Input : in String;
                      Output : out Bin_Array;
                      Output_Length : out Bin_Array_Index) is abstract
     with Pre'Class => (C.State = Ready and
                          Output'Length / Output_Group_Size(C) >=
                            Input'Length / Input_Group_Size(C) + 1);

   procedure Complete (C : in out Codec_To_Bin;
                        Output : out Bin_Array;
                        Output_Length : out Bin_Array_Index) is abstract
     with Pre'Class => (C.State = Ready and
                          Output'Length >= Output_Group_Size(C)),
       Post'Class => C.State in Completed | Failed;

   -- Helper functions

   generic
      type Codec is new Codec_To_String with private;
   function To_String (Input : in Bin_Array) return String;

   Invalid_Data_Encoding : exception;

   generic
      type Codec is new Codec_To_Bin with private;
   function To_Bin (Input : in String) return Bin_Array;

   -- Define Alphabet types

   subtype Alphabet_Index is Bin range 0..Bin'Last - 1;
   type Alphabet is array (Alphabet_Index range <>) of Character;

   function Valid_Alphabet (A : in Alphabet;
                            Case_Sensitive : in Boolean) return Boolean
     with Pre => (A'First = 0);

   subtype Alphabet_16 is Alphabet(0..15);
   subtype Alphabet_32 is Alphabet(0..31);
   subtype Alphabet_64 is Alphabet(0..63);
   subtype Alphabet_85 is Alphabet(0..84);

private

   type Reverse_Alphabet_Lookup is array (Character) of Bin;

   Invalid_Character_Input : constant Bin := 255;
   -- Any useful BinToAsc codec cannot map all Bin values to a Character value
   -- else there would be no benefit over simply using the Bin data directly.

   function Make_Reverse_Alphabet (A : in Alphabet;
                                   Case_Sensitive : in Boolean)
                                   return Reverse_Alphabet_Lookup
     with Pre => (Valid_Alphabet(A, Case_Sensitive));

   -- This compile-time check is useful for GNAT, but in GNATprove it currently
   -- just generates a warning that it can not yet be proved correct.
   pragma Warnings (GNATprove, Off, "Compile_Time_Error");
   pragma Compile_Time_Error ((Bin'Size /= 8 or
                                Bin'Modulus /= 256 or
                                  Bin'Last /= 255 or
                                    Bin'First /= 0),
                              "BinToAsc only works where the binary type" &
                                "specified is a regular 8-bit byte.");
   pragma Warnings (GNATprove, On, "Compile_Time_Error");

end BinToAsc;
