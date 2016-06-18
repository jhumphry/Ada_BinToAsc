-- RFC4648_Proof
-- Binary data to ASCII codecs specified in RFC4648

-- This version is a working copy for use as the adjustments needed to make the
-- SPARK proof work are made.

-- Copyright (c) 2015, James Humphry - see LICENSE file for details

-- with System.Storage_Elements;
with BinToAsc_Proof, BinToAsc_Proof.Base16;

package RFC4648_Proof
with Spark_Mode => On is

   package BToA renames BinToAsc_Proof;

   subtype Codec_State is BToA.Codec_State;
   function Ready return BToA.Codec_State renames BToA.Ready;
   function Complete return BToA.Codec_State renames BToA.Completed;
   function Failed return BToA.Codec_State renames BToA.Failed;

   Base16_Alphabet : constant BToA.Alphabet_16 := "0123456789ABCDEF";
   package Base16 renames BToA.Base16;
   package Hex renames Base16;
--
--     package Base16_Case_Insensitive is new BToA.Base16(Alphabet       => Base16_Alphabet,
--                                                        Case_Sensitive => False);
--
   Base32_Alphabet : constant BToA.Alphabet_32 :=
     "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567";
   package Base32 renames BToA.Base32;
--
--     package Base32_Case_Insensitive is new BToA.Base32(Alphabet         => Base32_Alphabet,
--                                                        Padding          => '=',
--                                                        Case_Sensitive   => False,
--                                                        Allow_Homoglyphs => False);
--
--     package Base32_Allow_Homoglyphs is new BToA.Base32(Alphabet         => Base32_Alphabet,
--                                                        Padding          => '=',
--                                                        Case_Sensitive   => False,
--                                                        Allow_Homoglyphs => True);
--
--     Base32Hex_Alphabet : constant BToA.Alphabet_32 :=
--       "0123456789ABCDEFGHIJKLMNOPQRSTUV";
--     package Base32Hex is new BToA.Base32(Alphabet         => Base32Hex_Alphabet,
--                                          Padding          => '=',
--                                          Case_Sensitive   => True,
--                                          Allow_Homoglyphs => False);
--
--     Base64_Alphabet : constant BToA.Alphabet_64 :=
--       "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
--
--     package Base64 is new BToA.Base64(Alphabet => Base64_Alphabet,
--                                       Padding  => '=');
--
--     Base64_URL_Safe_Alphabet : constant BToA.Alphabet_64 :=
--       "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_";
--
--     package Base64_URL_Safe is new BToA.Base64(Alphabet =>
--                                                   Base64_URL_Safe_Alphabet,
--                                                Padding  => '=');

end RFC4648_Proof;
