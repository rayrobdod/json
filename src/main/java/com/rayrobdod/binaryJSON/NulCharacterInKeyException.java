package com.rayrobdod.binaryJSON;

/**
 * BSON keys cannot contain '\0'. If something is trying to encode BSON
 * and encounters a '\0' in a key, it should throw this exception.
 */
public class NulCharacterInKeyException extends RuntimeException {
	
	public NulCharacterInKeyException(String detail, Throwable cause) {
		super(detail, cause);
	}
	public NulCharacterInKeyException(String detail) {
		super(detail);
	}
	public NulCharacterInKeyException() {
		super();
	}
}
