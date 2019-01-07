/**
 * @file PackedString.hpp
 * @author Daniel Starke
 * @date 2019-01-06
 * @version 2019-01-07
 * 
 * PackedString Copyright (c) 2019 Daniel Starke
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *  1. Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *  3. Neither the name of the copyright holder nor the names of its contributors
 *     may be used to endorse or promote products derived from this software
 *     without specific prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
 * THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS
 * BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY,
 * OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT
 * OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING
 * IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY
 * OF SUCH DAMAGE.
 * 
 * This library provides means to pack strings at compile time and unpack them at runtime. It is
 * optimized for compiled size on a ATmega328p where it incorporates a 1490 byte overhead for the
 * unpack routine when compiled with Arduino 1.8.5 and GCC 7.3.0. Make also sure that at least
 * window size + 100 bytes are available on the stack in case of an ATmega328p.  
 * Uncompressed strings are used as fallback if no C++14 compiler was detected.  
 * Simply usage example:  
 * @code{.cpp}
 * void setup() {
 * 	Serial.begin(115200);
 * 	while ( ! Serial ); 
 * 	UNPACK_STR(const char b, 4, "Hello World!") {
 * 		Serial.write(b);
 * 	}
 * }
 * @endcode
 * 
 * @remarks This code is optimized for small program size, not for speed.
 */
#ifndef __PACKEDSTRING_HPP__
#define __PACKEDSTRING_HPP__

extern "C" {

#include <limits.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#ifdef __AVR__
#include <avr/pgmspace.h>
#define PACKEDSTRING_ATTR_ROM PROGMEM
#define PACKEDSTRING_ROM_READ_U8(x, o) pgm_read_byte((x) + (o))
#define PACKEDSTRING_ROM_MEMCPY(dst, src, size) memcpy_P((dst), (src), (size))
#else /* !__AVR__ */
#define PACKEDSTRING_ATTR_ROM
#define PACKEDSTRING_ROM_READ_U8(x, o) (x)[o]
#define PACKEDSTRING_ROM_MEMCPY(dst, src, size) memcpy((dst), (src), (size))
#endif /* __AVR__ */

} /* extern "C" */


#define PACKEDSTRING_CAT3(x, y) x##y
#define PACKEDSTRING_CAT2(x, y) PACKEDSTRING_CAT3(x, y)
#define PACKEDSTRING_CAT(x, y) PACKEDSTRING_CAT2(x, y)
#define PACKEDSTRING_LOCAL(x) PACKEDSTRING_CAT(PACKEDSTRING_CAT(PACKEDSTRING_CAT(__LOCAL__, x), __), __LINE__)


#if __cpp_constexpr >= 201304
#define NEW_LZSSP_STR(ws, str) PackedString::LzsspString<sizeof(str), PackedString::packedSize<uint8_t(ws)>(str), uint8_t(ws)>(str)
#define LZSSP_STR_SIZE(ws, str) sizeof(NEW_LZSSP_STR(ws, str))
#define DEF_LZSSP_STR(var, ws, str) constexpr static const auto var PACKEDSTRING_ATTR_ROM = NEW_LZSSP_STR(ws, str)

#define UNPACK_STR_LZSSP_FOR(var, packedStr) \
	for (auto PACKEDSTRING_LOCAL(__i) = packedStr.begin(); PACKEDSTRING_LOCAL(__i); ++PACKEDSTRING_LOCAL(__i)) \
		for (bool PACKEDSTRING_LOCAL(__continue) = true; PACKEDSTRING_LOCAL(__continue); ) \
			for (var = *PACKEDSTRING_LOCAL(__i); PACKEDSTRING_LOCAL(__continue); PACKEDSTRING_LOCAL(__continue) = false)
	

/* use LZSSP compression */
#define UNPACK_STR_LZSSP(var, ws, str) \
	DEF_LZSSP_STR(PACKEDSTRING_LOCAL(__packed), ws, str); \
	UNPACK_STR_LZSSP_FOR(var, PACKEDSTRING_LOCAL(__packed))
#endif /* __cpp_constexpr */


#define UNPACK_STR_PLAIN_FOR(var, plainStr) \
	for (const char * PACKEDSTRING_LOCAL(__i) = plainStr; PACKEDSTRING_ROM_READ_U8(PACKEDSTRING_LOCAL(__i), 0) != 0; PACKEDSTRING_LOCAL(__i)++) \
		for (bool PACKEDSTRING_LOCAL(__continue) = true; PACKEDSTRING_LOCAL(__continue); ) \
			for (var = PACKEDSTRING_ROM_READ_U8(PACKEDSTRING_LOCAL(__i), 0); PACKEDSTRING_LOCAL(__continue); PACKEDSTRING_LOCAL(__continue) = false)

#define DEF_PLAIN_STR(var, str) static const char var[] PACKEDSTRING_ATTR_ROM = str

/* compression is not supported; fall back to plain text */
#define UNPACK_STR_PLAIN(var, str) \
	DEF_PLAIN_STR(PACKEDSTRING_LOCAL(__plain), str); \
	UNPACK_STR_PLAIN_FOR(var, PACKEDSTRING_LOCAL(__plain))

/**
 * @def DEF_PACKED_STR(var, ws, str)
 * Defines a variable holding a packed string.
 * 
 * @param var - variable name
 * @param ws - windows size in bytes (valid range is [2..255])
 * @param str - string literal to compress (non-literal strings are not allowed)
 */

/**
 * @def UNPACK_STR_FOR(var, packedStr)
 * Iterates over a packed string by unpacking each byte in a for loop.
 * 
 * @param var - loop variable declaration
 * @param packedStr - packed string variable
 */

/**
 * @def UNPACK_STR(var, ws, str)
 * Unpacks a string literal at runtime which was packed at compile time. Each unpacked character
 * is provided in a for loop.
 * 
 * @param var - loop variable declaration
 * @param ws - windows size in bytes (valid range is [2..255])
 * @param str - string literal to compress (non-literal strings are not allowed)
 */

/**
 * @def PACKED_STR_SIZE(ws, str)
 * Determines the size of the packed string literal not including helper functions and
 * distribution tables.
 * 
 * @param[in] ws - window size in bytes (valid range is [2..255])
 * @param[in] str - string literal to compress (non-literal strings are not allowed)
 */

/**
 * @def HAS_PACKEDSTRING
 * Defined if the packed string macros resolve to the packed variant. Not defined if they resolve
 * to the plain text variant.
 */
#if __cpp_constexpr >= 201304
#define DEF_PACKED_STR(var, ws, str) DEF_LZSSP_STR(var, ws, str)
#define UNPACK_STR_FOR(var, packedStr) UNPACK_STR_LZSSP_FOR(var, packedStr)
#define UNPACK_STR(var, ws, str) UNPACK_STR_LZSSP(var, ws, str)
#define PACKED_STR_SIZE(ws, str) LZSSP_STR_SIZE(ws, str)
#define HAS_PACKEDSTRING
#else /* !__cpp_constexpr */
#define DEF_PACKED_STR(var, ws, str) DEF_PLAIN_STR(var, ws, str)
#define UNPACK_STR_FOR(var, packedStr) UNPACK_STR_PLAIN_FOR(var, packedStr)
#define UNPACK_STR(var, ws, str) UNPACK_STR_PLAIN(var, str)
#define PACKED_STR_SIZE(ws, str) sizeof(str)
#undef HAS_PACKEDSTRING
#endif /* __cpp_constexpr */


#ifdef HAS_PACKEDSTRING
namespace PackedString {


enum {
	ShortLen = 16 /**< Optimize match lengths shorter than this by assuming that those occur more often than longer matches. */
};


/** Iterates over the bits of a byte array from LSB to MSB. */
class BitIterator {
private:
	uint8_t * const data;
	size_t size;
	size_t pos; /**< byte position */
	size_t offset; /**< bit offset */
public:
	/** Helper class to provide dereferencing functionality to BitIterator. */
	class BitIteratorRef {
	private:
		uint8_t * const data;
		size_t pos; /**< byte position */
		size_t offset; /**< bit offset */
		const bool valid; /* a bug in early GCC versions prevents us from checking data != NULL */
	public:
		/**
		 * Constructor.
		 * 
		 * @param[in] d - data pointer
		 * @param[in] p - byte position
		 * @param[in] o - bit offset
		 * @param[in] v - is data pointer valid?
		 */
		constexpr explicit BitIteratorRef(uint8_t * d, const size_t p, const size_t o, const bool v):
			data(d),
			pos(p),
			offset(o),
			valid(v)
		{}
		
		/**
		 * Returns the bit value.
		 * 
		 * @return bit value
		 */
		constexpr inline operator uint8_t() const {
			if ( ! this->valid ) return 0;
			return (this->data[this->pos] >> this->offset) & 0x01;
		}
		
		/**
		 * Sets the bit value.
		 * 
		 * @param[in] val - new value
		 * @return this object
		 */
		constexpr inline BitIteratorRef & operator= (const uint8_t val) {
			if ( ! this->valid ) return *this;
			this->data[this->pos] |= (static_cast<uint8_t>(val != 0) << (this->offset));
			return *this;
		}
	};

	/**
	 * Constructor.
	 * 
	 * @param[in] data - data pointer
	 * @param[in] len - data length
	 */
	constexpr explicit BitIterator(uint8_t * data, const size_t len):
		data(data),
		size(len),
		pos(0),
		offset(0)
	{}

	/**
	 * Not equal comparison.
	 * 
	 * @param[in] o - compare with this iterator instance
	 * @return true if not equal, else false
	 */
	constexpr inline bool operator!= (const BitIterator & o) const {
		return this->data != o.data || this->pos != o.pos || this->offset != o.offset;
	}

	/**
	 * Pre-increment operator.
	 * 
	 * @return this object
	 */
	constexpr inline BitIterator & operator++ () {
		++(this->offset);
		if (this->offset > 7) {
			++(this->pos);
			this->offset = 0;
		}
		return *this;
	}
	
	/**
	 * Dereferencing operator.
	 * 
	 * @return dereferenced bit
	 */
	constexpr inline BitIteratorRef operator* () {
		if (this->pos < this->size) return BitIteratorRef(this->data, this->pos, this->offset, true);
		return BitIteratorRef(NULL, 0, 0, false);
	}
	
	/**
	 * Returns the data pointer.
	 * 
	 * @return data pointer
	 */
	constexpr inline uint8_t * getData() const {
		return this->data;
	}
	
	/**
	 * Returns the current byte position.
	 * 
	 * @return byte position
	 */
	constexpr inline size_t getPos() const {
		return this->pos;
	}
	
	/**
	 * Returns the current bit offset.
	 * 
	 * @return bit offset
	 */
	constexpr inline size_t getOffset() const {
		return this->offset;
	}
};


/** Iterates over the bits of a byte array from LSB to MSB. */
class ConstBitIterator {
private:
	const uint8_t * const data;
	size_t size;
	size_t pos; /**< byte position */
	uint8_t offset; /**< bit offset */
public:
	/** Helper class to provide dereferencing functionality to BitIterator. */
	class BitIteratorRef {
	private:
		const bool val; /**< bit value */
	public:
		/**
		 * Constructor.
		 * 
		 * @param[in] value - bit value
		 */
		constexpr explicit BitIteratorRef(const bool value):
			val(value)
		{}
		
		/**
		 * Returns the current bit value.
		 * 
		 * @return bit value
		 */
		constexpr inline operator uint8_t() const {
			return static_cast<uint8_t>(this->val);
		}
	};

	/**
	 * Constructor.
	 * 
	 * @param[in] array - source data pointer
	 * @param[in] len - data length
	 */
	constexpr explicit ConstBitIterator(const uint8_t * array, const size_t len):
		data(array),
		size(len),
		pos(0),
		offset(0)
	{}

	/**
	 * Not equal comparison.
	 * 
	 * @param[in] o - compare with this iterator instance
	 * @return true if not equal, else false
	 */
	constexpr inline bool operator!= (const ConstBitIterator & o) const {
		return this->data != o.data || this->pos != o.pos;
	}

	/**
	 * Pre-increment operator.
	 * 
	 * @return this object
	 */
	constexpr inline ConstBitIterator & operator++ () {
		if (++(this->offset) > 7) {
			++(this->pos);
			this->offset = 0;
		}
		return *this;
	}
	
	/**
	 * Dereferencing operator.
	 * 
	 * @return dereferenced bit
	 */
	BitIteratorRef operator* () const __attribute__((noinline)) {
		if (this->pos < this->size) {
			return BitIteratorRef(static_cast<bool>((PACKEDSTRING_ROM_READ_U8(this->data, this->pos) >> this->offset) & 0x01));
		}
		return BitIteratorRef(false);
	}
	
	/**
	 * Returns the data pointer.
	 * 
	 * @return data pointer
	 */
	constexpr inline const uint8_t * getData() const {
		return this->data;
	}
	
	/**
	 * Returns the current byte position.
	 * 
	 * @return byte position
	 */
	constexpr inline size_t getPos() const {
		return this->pos;
	}
	
	/**
	 * Returns the current bit offset.
	 * 
	 * @return bit offset
	 */
	constexpr inline size_t getOffset() const {
		return static_cast<size_t>(this->offset);
	}
};


/**
 * Arithmetic encoding implementation using the range coding approach.
 * 
 * @see https://web.archive.org/web/20151022055156/http://www.sable.mcgill.ca/publications/techreports/2007-5/bodden-07-arithmetic-TR.pdf
 */
class RangeEncoder {
public:
	typedef uint8_t SymbolType;
	typedef uint16_t ValueType;
	typedef uint32_t RangeType;
	enum { RangeMin = 0, RangeMax = UINT32_MAX, RangeBits = 32 };
	enum {
		/** Half of the maximum for high. */
		Half = static_cast<RangeType>(1) << (RangeBits - 2),
		/** 1/4 of the maximum for high. */
		FirstQuarter = static_cast<RangeType>(1) << (RangeBits - 3),
		/** 3/4 of the maximum for high. */
		ThirdQuarter = static_cast<RangeType>(3) << (RangeBits - 3),
		/** Maximal possible range value. */
		MaxValue = (static_cast<RangeType>(1) << (RangeBits >> 1)) - 1
	};
private:
	/** Current lower boundary limit. */
	RangeType low;
	/** Current upper boundary limit. */
	RangeType high;
	/** Indicated how many bits the current range needs to be extended. */
	uint8_t scale;
	/** Bit output iterator. */
	BitIterator iter;
public:
	/**
	 * Constructor.
	 * 
	 * @param[out] out - output uint8_t array
	 * @param[in] outLen - output array size
	 */
	constexpr explicit RangeEncoder(uint8_t * out, const size_t outLen):
		low(0),
		high((static_cast<RangeType>(1) << (RangeBits - 1)) - 1),
		scale(0),
		iter(out, outLen)
	{}
	
	/**
	 * Returns the maximum possible range. Input values need to be limited to this range.
	 * 
	 * @return maximal range
	 */
	constexpr ValueType getMaxValue() const {
		return MaxValue;
	}
	
	/**
	 * Encode a single value within the given range and write the encoded data to the
	 * output. The method assumes a uniform distribution.
	 * 
	 * @param[in] value - value to encode
	 * @param[in] range - value range to apply (number of possible values)
	 * @return true on success, else false 
	 * @remarks range defines the interval [0, range) which means for example, that it needs to be
	 * 256 if the values from 0 to 255 are to be encoded.
	 */
	constexpr void encode(const SymbolType value, const ValueType range) {
		this->encode(static_cast<ValueType>(value), static_cast<ValueType>(value) + 1, range);
	}
	
	/**
	 * Encode a span within the given range and write the encoded data to the
	 * output.
	 * 
	 * @param[in] lowerBound - lower span bound to encode
	 * @param[in] upperBound - upper span bound to encode
	 * @param[in] range - value range to apply (highest upper bound value)
	 * @return true on success, else false 
	 * @remarks range defines the interval [0, range) which means for example, that it needs to be
	 * 256 if the values from 0 to 255 are to be encoded.
	 */
	constexpr void encode(const ValueType lowerBound, const ValueType upperBound, const ValueType range) {
		/* partition number space into single steps */
		const RangeType step = (this->high - this->low + 1) / range; /* interval open at the top => +1 */
		this->high = this->low + (step * upperBound) - 1; /* interval open at the top => -1 */
		this->low = this->low + (step * lowerBound);

		/* while the MSB of high and low match, output them and rescale low and high */
		while ((this->high < Half) || (this->low >= Half)) {
			if (this->high < Half) {
				*(this->iter) = 0;
				++(this->iter);
				this->low = this->low << 1;
				this->high = (this->high << 1) | 1;

				for (; this->scale > 0; this->scale--) {
					*(this->iter) = 1;
					++(this->iter);
				}
			} else if (this->low >= Half) {
				*(this->iter) = 1;
				++(this->iter);
				this->low = (this->low - Half) << 1;
				this->high = ((this->high - Half) << 1) | 1;

				for (; this->scale > 0; this->scale--) {
					*(this->iter) = 0;
					++(this->iter);
				}
			}
		}

		/* if there is a danger of underflow, increase the underflow counter and rescale low and high */
		while ((FirstQuarter <= this->low) && (this->high < ThirdQuarter)) {
			this->scale++;
			this->low = (this->low - FirstQuarter) << 1;
			this->high = ((this->high - FirstQuarter) << 1) | 1;
		}
	}

	/**
	 * Needs to be called after all values have been encoded to ensure that the result complete.
	 * 
	 * @return true on success, else false 
	 */
	constexpr bool finalize() {
		/* There are two possibilities of how low and high can be distributed,
		   which means that two bits are enough to distinguish them. */
		if (this->low < FirstQuarter) { /* low < firstQuarter < half <= high */
			*(this->iter) = 0;
			++(this->iter);
			for (RangeType i = 0; i <= this->scale; i++) {
				*(this->iter) = 1;
				++(this->iter);
			}
		} else { /* low < half < thirdQuarter <= high */
			*(this->iter) = 1; /* zeros added automatically by the decoder; no need to store them */
			++(this->iter);
		}
		return true;
	}
	
	/**
	 * Returns the number of octets encoded.
	 * 
	 * @return encoded size in octets
	 */
	constexpr size_t getEncodedSize() const {
		return this->iter.getPos() + static_cast<size_t>(this->iter.getOffset() > 0);
	}
};


/**
 * Arithmetic decoding implementation using the range coding approach.
 * 
 * @see RangeEncoder
 */
class RangeDecoder {
public:
	typedef uint8_t SymbolType;
	typedef uint16_t ValueType;
	typedef uint32_t RangeType;
	typedef SymbolType (* RangeGetter)(ValueType & lowerBound, ValueType & upperBound, const ValueType matchValue, void * user);
	enum { RangeMin = 0, RangeMax = UINT32_MAX, RangeBits = 32 };
	enum {
		/** Half of the maximum for high. */
		Half = static_cast<RangeType>(1) << (RangeBits - 2),
		/** 1/4 of the maximum for high. */
		FirstQuarter = static_cast<RangeType>(1) << (RangeBits - 3),
		/** 3/4 of the maximum for high. */
		ThirdQuarter = static_cast<RangeType>(3) << (RangeBits - 3),
		/** Maximal possible range value. */
		MaxValue = (static_cast<RangeType>(1) << (RangeBits >> 1)) - 1
	};
private:
	/** Current lower boundary limit. */
	RangeType low;
	/** Current upper boundary limit. */
	RangeType high;
	/** Buffered input value to decode values from its ranges. */
	RangeType buffer;
	/** Bit input iterator. */
	ConstBitIterator iter;
public:
	/**
	 * Constructor.
	 * 
	 * @param[in] in - input uint8_t array
	 * @param[in] inLen - input array size
	 */
	constexpr explicit RangeDecoder(const uint8_t * in, const size_t inLen):
		low(0),
		high((static_cast<RangeType>(1) << (RangeBits - 1)) - 1),
		buffer(0),
		iter(in, inLen)
	{
		/* fill buffer with bits from the input */
		for (uint8_t i = (RangeBits - 1); i > 0; i--) {
			this->buffer = (this->buffer << 1) | *(this->iter);
			++(this->iter);
		}
	}
	
	/**
	 * Returns the maximum possible range. Input values need to be limited to this range.
	 * 
	 * @return maximal range
	 */
	inline RangeType getMaxValue() const {
		return MaxValue;
	}
	
	/**
	 * Decode a single value from the input and return it.
	 * The method assumes a uniform distribution.
	 * 
	 * @param[in] range - value range to apply (number of possible values)
	 * @return decoded symbol
	 * @remarks range defines the interval [0, range) which means for example, that it needs to be
	 * 256 if the values from 0 to 255 are to be decoded.
	 */
	inline SymbolType decode(const ValueType range) {
		return this->decode(range, RangeDecoder::map, NULL);
	}
	
	
	/**
	 * Decode a single value from the input and return it.
	 * The method assumes a uniform distribution.
	 * 
	 * @param[in] range - value range to apply (number of possible values)
	 * @param[in] retriever - callback function to map ranges to symbols
	 * @param[in,out] user - user defined value passed to the given callback function
	 * @return decoded symbol
	 * @remarks range defines the interval [0, range) which means for example, that it needs to be
	 * 256 if the values from 0 to 255 are to be decoded.
	 */
	__attribute__((noinline)) SymbolType decode(const ValueType range, RangeGetter retriever, void * user) {
		/* determine value */
		const RangeType step = RangeDecoder::div(this->high - this->low + 1, range); /* interval open at the top => +1 */
		ValueType lowerBound, upperBound;
		const SymbolType value = (*retriever)(lowerBound, upperBound, RangeDecoder::div(this->buffer - this->low, step), user);
	    
		/* update upper bound */
		this->high = this->low + (step * upperBound) - 1; /* interval open at the top => -1 */
		/* update lower bound */
		this->low = this->low + (step * lowerBound);
		
		/* while the MSB of high and low match, output them and rescale low and high */
		while ((this->high < Half) || (this->low >= Half)) {
			if (this->high < Half) {
				this->rescale(0);
			} else {
				this->rescale(Half);
			}
		}
	
		/* if there is a danger of underflow, increase the underflow counter and rescale low and high */
		while ((FirstQuarter <= this->low) && (this->high < ThirdQuarter)) {
			this->rescale(FirstQuarter);
		}
		
		return value;
	}
	
private:
	/**
	 * Callback to return the range from a value within the range.
	 * 
	 * @param[out] lowerBound - set to the lower bound
	 * @param[out] upperBound - set to the upper bound
	 * @param[in] matchValue - value within a range (determine range from this value)
	 * @param[in,out] user - user defined callback value
	 * @return decoded value
	 */
	static SymbolType map(ValueType & lowerBound, ValueType & upperBound, const ValueType matchValue, void *) __attribute__((noinline)) {
		lowerBound = matchValue;
		upperBound = matchValue + 1;
		return static_cast<SymbolType>(matchValue);
	}
	
	/**
	 * Helper function to rescale the low/high/buffer values by reading in the next bit.
	 * 
	 * @param[in] offset - range offset
	 */
	__attribute__((noinline)) void rescale(const RangeType offset) {
		this->low = (this->low - offset) << 1;
		this->high = ((this->high - offset) << 1) | 1;
		this->buffer = ((this->buffer - offset) << 1) | *(this->iter);
		++(this->iter);
	}
	
	/**
	 * Helper function to reduce code usage for 32-bit division.
	 * 
	 * @param[in] lhs - left-hand statement
	 * @param[in] rhs - right-hand statement
	 * @return lhs / rhs
	 */
	__attribute__((noinline)) static RangeType div(const RangeType lhs, const RangeType rhs) {
		return lhs / rhs;
	}
};


/**
 * Checks the length of the match at the given positions.
 * 
 * @param[in] v - match within this string
 * @param[in] l - length of v
 * @param[in] s - source offset (find match from here until one prior to i)
 * @param[in] i - destination offset (match against string starting at this position)
 * @param[in] ws - window size
 * @return length of the match
 */
constexpr static size_t match(const char * v, const size_t l, const size_t s, const size_t i, const uint8_t ws) {
	size_t res = 0;
	for (size_t e = s, j = i; j < l && res < ws && v[e] == v[j]; j++, e++, res++) {
		if (e >= i) e = s;
	}
	return res;
}


/**
 * Encode symbol and update prediction values (i.e. symbol distribution table).
 * 
 * @param[in] enc - encoder instance to use
 * @param[in] index - symbol to encode (index to the distribution table)
 * @param[in] pred - prediction/distribution table
 * @param[in] count - variable holding the sum of all prediction/distribution table entries
 * @param[in] update - true to update the table entries, false to skip the update
 */
constexpr static void encodeAndUpdatePred(RangeEncoder & enc, const size_t index, uint16_t * pred, uint16_t & count, const bool update) {
	typedef RangeEncoder::ValueType ValueType;
	size_t i = 0;
	ValueType lowerBound = 0;
	for (; i < index; i++) lowerBound += static_cast<ValueType>(pred[i]);
	enc.encode(lowerBound, static_cast<ValueType>(lowerBound + pred[i]), static_cast<ValueType>(count));
	if ( update ) {
		pred[i]++;
		count++;
		if (count >= UINT16_MAX) {
			/* adjust counts on overflow but keep values > 0 to stay greater than zero */
			for (uint16_t * p = pred; p != &count; p++) {
				count = static_cast<uint16_t>(count + 1 - (*p >> 1) - (*p & 1));
				*p = (*p >> 1) + 1;
			}
		}
	}
}


/* forward declarations */
struct Distribution;

template <size_t N, size_t P>
constexpr static size_t pack(const char (& v)[N], unsigned char (& out)[P], const uint8_t ws, Distribution & dist, const bool update = false);


/**
 * Prediction/symbol distribution table.
 */
struct Distribution {
	uint16_t type[3]; /**< Distribution of the type flag. */
	uint16_t literal[9]; /**< Distribution of the literal class. */
	uint16_t len[3]; /**< Distribution of the match length value. */
	
	/**
	 * Constructor.
	 * 
	 * @param[in] v - string to pack
	 * @param[in] WS - window size in bytes
	 * @tparam N - length of v
	 */
	template <size_t N>
	constexpr explicit Distribution(const char (& v)[N], const uint8_t WS):
		type{0},
		literal{0},
		len{0}
	{
		unsigned char dummy[] = {0};
		Distribution::init(this->type);
		Distribution::init(this->literal);
		Distribution::init(this->len);
		pack(v, dummy, WS, *this, true);
		Distribution::fix(this->type);
		Distribution::fix(this->literal);
		Distribution::fix(this->len);
	}
	
	/**
	 * Initializes the given distribution table with 1.
	 * 
	 * @param[in] a - table to initialize
	 * @tparam N - size of a
	 */
	template <size_t N>
	constexpr static void init(uint16_t (& a)[N]) {
		for (size_t i = 0; (i + 1) < N; i++) a[i] = 1;
		a[N - 1] = N - 1;
	}
	
	/**
	 * Adjusts the distribution table entries by removing/subtracting initialized values.
	 * 
	 * @param[in] a - table to fix
	 * @tparam N - size of a
	 */
	template <size_t N>
	constexpr static void fix(uint16_t (& a)[N]) {
		a[N - 1] = 0;
		for (size_t i = 0; (i + 1) < N; i++) {
			a[i]--;
			a[N - 1] += a[i];
		}
	}
};


/**
 * Temporary prediction/symbol distribution table. This is needed to generate a in memory copy from
 * the distribution table stored in program space (i.e. flash on a ATmega328p for example).
 */
struct TempDistribution {
	uint16_t type[3];
	uint16_t literal[9];
	uint16_t len[3];
	
	/**
	 * Constructor.
	 * 
	 * @param[in] o - Distribution table to clone
	 */
	explicit TempDistribution(const Distribution & o) {
		PACKEDSTRING_ROM_MEMCPY(this, &o, sizeof(Distribution));
	}
};


/**
 * Packs the given string and writes the compressed data to the passed output array. Distribution
 * tables are used to optimize the compression rate using a prediction by occurrence probability.
 * 
 * @param[in] v - data to pack
 * @param[out] out - array to store the packed data
 * @param[in] ws - window size in the range [2..255]
 * @param[in,out] dist - distribution tables
 * @param[in] update - flag whether to update the distribution tables with the found probabilities
 * @return packed data size in bytes
 * @tparam N - size of v
 * @tparam P - size of out
 */
template <size_t N, size_t P>
constexpr static size_t pack(const char (& v)[N], unsigned char (& out)[P], const uint8_t ws, Distribution & dist, const bool update) {
	typedef RangeEncoder::ValueType ValueType;
	if (ws <= 0 || ws > 256) return 0;
	RangeEncoder enc(out, P);
	/* encode literal */
	encodeAndUpdatePred(enc, 0, dist.type, dist.type[2], update);
	encodeAndUpdatePred(enc, static_cast<size_t>(*v) >> 5, dist.literal, dist.literal[8], update);
	enc.encode(static_cast<ValueType>(*v) & 0x1F, static_cast<ValueType>(32));
	for (size_t i = 1, s = 0; i < N;) {
		if ((s + ws) <= i) s = i - ws + 1;
		/* find longest match */
		size_t pos = 0;
		size_t len = 0;
		for (size_t j = s; j < i; j++) {
			const size_t newLen = match(v, N, j, i, ws);
			if (newLen > len) {
				pos = i - j;
				len = newLen;
			}
		}
		/* shorten longest match in favor for longer double matches */
		if (len > 1) {
			size_t optLen = 0;
			size_t maxLen = len;
			for (size_t k = i + 1; k < (i + len); k++) {
				const size_t b = (k > ws) ? k - ws + 1 : 0;
				for (size_t j = b; j < k; j++) {
					const size_t newLen = match(v, N, j, k, ws);
					const size_t span = k - i;
					if (newLen > span && (newLen + span) > maxLen) {
						optLen = span;
						maxLen = len + optLen;
					}
				}
			}
			if (optLen > 0) len = optLen;
		}
		/* limit length by decode buffer size */
		if (len > 2) {
			/* encode match as (pos, len) */
			encodeAndUpdatePred(enc, 1, dist.type, dist.type[2], update);
			enc.encode(static_cast<ValueType>(pos), static_cast<ValueType>(ws));
			if (len < ShortLen) {
				encodeAndUpdatePred(enc, 0, dist.len, dist.len[2], update);
				enc.encode(static_cast<ValueType>(len), static_cast<ValueType>(ShortLen));
			} else {
				encodeAndUpdatePred(enc, 1, dist.len, dist.len[2], update);
				enc.encode(static_cast<ValueType>(len - ShortLen), static_cast<ValueType>(ws - ShortLen));
			}
			i += len;
		} else {
			/* encode literal */
			encodeAndUpdatePred(enc, 0, dist.type, dist.type[2], update);
			encodeAndUpdatePred(enc, static_cast<size_t>(v[i]) >> 5, dist.literal, dist.literal[8], update);
			enc.encode(static_cast<ValueType>(v[i]) & 0x1F, static_cast<ValueType>(32));
			i++;
		}
	}
	enc.finalize();
	return enc.getEncodedSize();
}


/**
 * Helper class to provide a simplified iterator which unpacks the packed string data.
 */
class UnpackIteratorBase {
private:
	typedef RangeDecoder::SymbolType SymbolType;
	typedef RangeDecoder::ValueType ValueType;
	const size_t P; /**< packed data size */
	size_t p; /**< unpacked data size at this point */
	uint8_t pos; /**< match position */
	uint8_t len; /**< match length */
	uint8_t j; /**< match index */
	uint8_t limit; /**< match index limit */
	uint8_t state; /**< unpack state machine state */
	char val; /**< last value unpacked */
	TempDistribution dist; /**< distribution tables */
	RangeDecoder dec; /**< decoder instance */
public:
	/**
	 * Constructor.
	 * 
	 * @param[in] cData - packed data
	 * @param[in] cSize - packed data size
	 * @param[in] pSize - unpacked data size
	 * @param[in] distribution - distribution tables
	 */
	explicit UnpackIteratorBase(const unsigned char * cData, const size_t cSize, const size_t pSize, const TempDistribution & distribution):
		P(pSize),
		p(0),
		pos(0),
		len(0),
		j(0),
		limit(0),
		state(0),
		val(0),
		dist(distribution),
		dec(cData, cSize)
	{}
	
	/**
	 * Reached the end of the packed data?
	 * 
	 * @return true if not at the end yet, else false
	 */
	inline operator bool () const {
		return this->p < this->P;
	}
	
	/**
	 * Maps the decoded range value to a symbol. Adjusts the lower and upper bounds values for the
	 * decoder.
	 * 
	 * @param[out] lowerBound - lower bound value for the next symbol (provided to the decoder)
	 * @param[out] upperBound - upper bound value for the next symbol (provided to the decoder)
	 * @param[in] matchValue - range value to map
	 * @param[in] user - user defined data (the distribution table to use)
	 * @return mapped symbol
	 */
	static SymbolType map(ValueType & lowerBound, ValueType & upperBound, const ValueType matchValue, void * user) __attribute__((noinline)) {
		const uint16_t * pred = reinterpret_cast<const uint16_t *>(user);
		SymbolType i = 0;
		for (lowerBound = 0; (lowerBound + pred[i]) <= matchValue; i++) {
			lowerBound += static_cast<ValueType>(pred[i]);
		}
		upperBound = static_cast<ValueType>(lowerBound + pred[i]);
		return i;
	}
	
	/**
	 * Decodes the next character.
	 * 
	 * @param[in] window - window buffer to use
	 * @param[in] ws - window size
	 */
	void next(char * window, const uint8_t ws) __attribute__((noinline)) {
		switch (state) {
		case 0:
			/* decode type */
			state = static_cast<int>(dec.decode(dist.type[2], UnpackIteratorBase::map, dist.type)) + 1;
			this->next(window, ws);
			break;
		case 1:
			/* decode literal */
			val = static_cast<char>(dec.decode(dist.literal[8], UnpackIteratorBase::map, dist.literal));
			val = static_cast<char>((static_cast<uint8_t>(val) << 5) | dec.decode(32));
			window[UnpackIteratorBase::mod(p, ws)] = val;
			p++;
			state = 0;
			break;
		case 2:
			/* decode match as (pos, len) */
			pos = dec.decode(ws);
			if (dec.decode(dist.len[2], UnpackIteratorBase::map, dist.len) == 0) {
				len = dec.decode(ShortLen);
			} else {
				len = dec.decode(ws - ShortLen) + ShortLen;
			}
			limit = (p < ws) ? p : ws;
			state = 3;
			/* fall through */
		default:
			/* process match */
			if (j < len) {
				val = window[UnpackIteratorBase::mod((p - pos + UnpackIteratorBase::mod(j, pos)), limit)];
				window[UnpackIteratorBase::mod((p + j), ws)] = val;
				j++;
			} else {
				/* end of match; proceed with next character */
				j = 0;
				p += len;
				state = 0;
				this->next(window, ws);
			}
			break;
		}
	}
	
	/**
	 * Returns the current iterator value.
	 * 
	 * @return decoded character
	 */
	inline char operator* () const {
		return this->val;
	}
	
	/**
	 * Returns the current iterator position/number of decoded characters.
	 * 
	 * @return number of decoded characters
	 */
	inline size_t getUnpackedPos() const {
		return this->p + this->j;
	}
	
private:
	/**
	 * Calculates the modulo. Helper function to decrease the compiled code size.
	 * 
	 * @param[in] lhs - left-hand statement
	 * @param[in] rhs - right-hand statement
	 * @return lhs % rhs
	 */
	inline static uint8_t mod(const size_t lhs, const uint8_t rhs) {
		return lhs % rhs;
	}
};


/**
 * Helper class to provide a simplified iterator which unpacks the packed string data.
 * 
 * @tparam WS - window size
 */
template <uint8_t WS>
class UnpackIterator : public UnpackIteratorBase {
private:
	char window[WS]; /**< Reserves the needed space for the window buffer. */
public:
	/**
	 * Constructor.
	 * 
	 * @param[in] cData - packed data
	 * @param[in] cSize - packed data size
	 * @param[in] pSize - unpacked data size
	 * @param[in] distribution - distribution tables
	 */
	explicit UnpackIterator(const unsigned char * cData, const size_t cSize, const size_t pSize, const TempDistribution & distribution):
		UnpackIteratorBase(cData, cSize, pSize, distribution)
		/* window remains uninitialized */
	{
		this->UnpackIteratorBase::next(this->window, WS);
	}
	
	using UnpackIteratorBase::operator bool;
	using UnpackIteratorBase::operator*;
	using UnpackIteratorBase::getUnpackedPos;
	
	/**
	 * Decodes the next character.
	 * 
	 * @return reference to this
	 */
	inline UnpackIterator & operator++ () {
		this->UnpackIteratorBase::next(this->window, WS);
		return *this;
	}
};


/**
 * Object holding a LZSSP packed string and the corresponding distribution tables.
 * 
 * @tparam P - unpacked size
 * @tparam C - packed size
 * @tparam WS - window size
 */
template <size_t P, size_t C, uint8_t WS>
struct LzsspString {
	const Distribution dist; /**< distribution tables used */
	unsigned char data[C]; /**< packed data */
	
	/**
	 * Constructor.
	 * 
	 * @param[in] v - string to pack
	 * @remarks This should be processed at compile time (i.e. used as constexpr).
	 */
	constexpr explicit LzsspString(const char (& v)[P]):
		dist(v, WS),
		data{0}
	{
		Distribution tmpDist(this->dist);
		pack(v, this->data, WS, tmpDist);
	}
	
	/**
	 * Returns the packed size.
	 * 
	 * @return packed size
	 */
	constexpr inline size_t size() const {
		return C;
	}
	
	/**
	 * Returns the unpacked size.
	 * 
	 * @return unpacked size
	 */
	constexpr inline size_t unpackedSize() const {
		return P;
	}
	
	/**
	 * Returns an iterator instance to the beginning of the packed data.
	 * 
	 * @return start iterator
	 */
	inline UnpackIterator<WS> begin() const {
		TempDistribution tmpDist(this->dist);
		return UnpackIterator<WS>(this->data, C, P, tmpDist);
	}
};


/**
 * Calculates the packed data size from the given string.
 * 
 * @param[in] v - string to pack
 * @return packed data size
 * @remarks This should be processed at compile time (i.e. used as constexpr).
 */
template <uint8_t WS, size_t N>
constexpr static size_t packedSize(const char (& v)[N]) {
	unsigned char dummy[] = {0};
	Distribution dist(v, WS);
	return pack(v, dummy, WS, dist);
}


} /* namespace PackedString */
#endif /* HAS_PACKEDSTRING */


#endif /* __PACKEDSTRING_HPACKEDSTRING__ */
