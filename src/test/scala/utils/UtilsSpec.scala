package utils

import org.scalatest.FunSpec

class UtilsSpec extends FunSpec {
  describe("hexToBase64") {
    describe("when the passed input's length modulo six is not zero") {
      describe("when the tail is of length 1") {
        it("returns the correct value for short inputs") {
          assert(hexToBase64("1") == "AQ==")
        }

        it("returns the correct value for long inputs") {
          assert(hexToBase64("F5678A2") == "D1Z4og==")
        }

        it("handles mixed case") {
          assert(hexToBase64("f5678A2") == "D1Z4og==")
        }
      }

      describe("when the tail is of length 2") {
        it("returns the correct value for short inputs") {
          assert(hexToBase64("01") == "AQ==")
        }

        it("returns the correct value for long inputs") {
          assert(hexToBase64("0F5678A2") == "D1Z4og==")
        }

        it("handles mixed case") {
          assert(hexToBase64("0f5678A2") == "D1Z4og==")
        }
      }
    }

    describe("when the passed input's length modulo six is zero") {
      it("returns the correct value for short inputs") {
        val firstInput = "49276D"
        val firstOutput = "SSdt"
        assert(hexToBase64(firstInput) == firstOutput)
      }

      it("returns the correct value for long inputs") {
        val firstInput = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
        val firstOutput = "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"
        assert(hexToBase64(firstInput) == firstOutput)
      }

      it("returns the correct value for very long inputs") {
        val firstInput = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d" * 100
        val firstOutput = "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t" * 100
        assert(hexToBase64(firstInput) == firstOutput)
      }

      it("handles mixed case") {
        val firstInput = "49276d"
        val firstOutput = "SSdt"
        assert(hexToBase64(firstInput) == firstOutput)
      }
    }
  }

  describe("bufferXor") {
    describe("when passed a String and another String of the same length") {
      it("returns the correct value") {
        val firstInput = "1c0111001f010100061a024b53535009181c"
        val secondInput = "686974207468652062756c6c277320657965"
        val output = "746865206b696420646f6e277420706c6179"
        assert(bufferXor(firstInput, secondInput) == output)
      }
    }

    describe("when passed a String and a single Char") {
      it("returns the correct value") {
        val inputString = "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"
        val inputChar = 'a'
        val output = "zVVRPW^\u0019tz\u001EJ\u0019UPR\\\u0019X\u0019IVLW]\u0019V_\u0019[XZVW"
        assert(bufferXor(inputString, inputChar) == output)
      }
    }
  }

  describe("englishAnalysisScore") {
    describe("when passed a single word") {
      it("returns a higher value for an english word") {
        val nonsenseScore = englishAnalysisScore("aofjeiwlfjdkslfj")
        assert(englishAnalysisScore("sentimental") > nonsenseScore)
      }

      it("returns a higher value for an english sentence") {
        val nonsenseScore = englishAnalysisScore("a f feowijef fdoijs elkfjqwe f doijf")
        assert(englishAnalysisScore("A small brow fox when running through the woods.") > nonsenseScore)
      }
    }
  }

  describe("deceipherXorCipher") {
    describe("when passed a string and a single character") {
      it("returns the correct value") {
        val input = "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"
        assert(deceipherXor(input) == "Cooking MC's like a pound of bacon")
      }
    }
  }
}
