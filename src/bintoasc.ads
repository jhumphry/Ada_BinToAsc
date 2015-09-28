-- BinToAsc
-- Binary data to ASCII codecs

-- Copyright (c) 2015, James Humphry - see LICENSE file for details

generic
   type Bin is mod <>;
   type Bin_Array_Index is range <>;
   type Bin_Array is array (Bin_Array_Index range <>) of Bin;
package BinToAsc is

   type Codec_State is (Ready, Complete, Failed);

   type Codec is abstract tagged record
      State : Codec_State := READY;
   end record;

   procedure Reset (C : out Codec) is abstract
     with Post'Class => (C.State = READY);
   -- Reset a Codec to its initial state

   type Codec_To_String is abstract new Codec with null record;

   function Expansion_Numerator (C : in Codec_To_String)
                                 return Positive is abstract;

   function Expansion_Denominator (C : in Codec_To_String)
                                   return Positive is abstract;

   function Maximum_Padding (C : in Codec_To_String)
                             return Natural is abstract;

   procedure Process (C : in out Codec_To_String;
                      Input : in Bin;
                      Output : out String;
                      Output_Length : out Natural) is abstract
     with Pre'Class => (C.State = Ready and
                          Output'Length >= Expansion_Numerator(C) /
                            Expansion_Denominator(C) + Maximum_Padding(C));

   procedure Process (C : in out Codec_To_String;
                      Input : in Bin_Array;
                      Output : out String;
                      Output_Length : out Natural) is abstract
     with Pre'Class => (C.State = Ready and
                          Output'Length >= (Input'Length * Expansion_Numerator(C))
                        / Expansion_Denominator(C) + Maximum_Padding(C));

   procedure Completed (C : in out Codec_To_String;
                        Output : out String;
                        Output_Length : out Natural) is abstract
     with Pre'Class => (C.State = Ready and
                          Output'Length >= Expansion_Numerator(C) /
                            Expansion_Denominator(C) + Maximum_Padding(C)),
       Post'Class => C.State in Complete | Failed;

   type Codec_To_Bin is abstract new Codec with null record;

   function Compression_Numerator (C : in Codec_To_Bin)
                                 return Positive is abstract;

   function Compression_Denominator (C : in Codec_To_Bin)
                                   return Positive is abstract;

   function Maximum_Padding (C : in Codec_To_Bin)
                             return Natural is abstract;

   procedure Process (C : in out Codec_To_Bin;
                      Input : in Character;
                      Output : out Bin_Array;
                      Output_Length : out Bin_Array_Index) is abstract
     with Pre'Class => (C.State = Ready and
                          Output'Length >= Compression_Numerator(C) /
                            Compression_Denominator(C) + Maximum_Padding(C));

   procedure Process (C : in out Codec_To_Bin;
                      Input : in String;
                      Output : out Bin_Array;
                      Output_Length : out Bin_Array_Index) is abstract
     with Pre'Class => (C.State = Ready and
                          Output'Length >= (Input'Length * Compression_Numerator(C))
                        / Compression_Denominator(C) + Maximum_Padding(C));

   procedure Completed (C : in out Codec_To_Bin;
                        Output : out Bin_Array;
                        Output_Length : out Bin_Array_Index) is abstract
     with Pre'Class => (C.State = Ready and
                          Output'Length >= Compression_Numerator(C) /
                            Compression_Denominator(C) + Maximum_Padding(C)),
       Post'Class => C.State in Complete | Failed;

   -- Define Alphabet types

   subtype Alphabet_Index is Integer range 0..254;
   type Alphabet is array (Alphabet_Index range <>) of Character;

   function Valid_Alphabet (A : in Alphabet;
                            Case_Sensitive : in Boolean) return Boolean
        with Pre => (A'First = 0);

   subtype Alphabet_16 is Alphabet(0..15);
   subtype Alphabet_32 is Alphabet(0..31);
   subtype Alphabet_64 is Alphabet(0..63);

   type Reverse_Alphabet_Lookup is array (Character) of Bin;

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
