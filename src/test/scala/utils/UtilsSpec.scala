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
        val firstInput = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d" * 10000
        val firstOutput = "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t" * 10000
        assert(hexToBase64(firstInput) == firstOutput)
      }

      it("handles mixed case") {
        val firstInput = "49276d"
        val firstOutput = "SSdt"
        assert(hexToBase64(firstInput) == firstOutput)
      }
    }
  }
}
