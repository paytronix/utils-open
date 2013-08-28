//
// Copyright 2003-2012 Paytronix Systems, Inc.
// All Rights Reserved
//

package com.paytronix.utils.internal.java;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.security.GeneralSecurityException;
import java.security.Security;
import java.util.Properties;

import javax.crypto.Cipher;
import javax.crypto.SecretKey;
import javax.crypto.SecretKeyFactory;
import javax.crypto.spec.DESedeKeySpec;
import javax.crypto.spec.IvParameterSpec;

// Scala to generate random 8-byte keys (a1, a2, a3, and so on):

/*
  val r = new java.util.Random()
  def mkkey = (
      (1 to 8)
      map (_ => (r.nextInt % 256).asInstanceOf[Byte])
      map (i => String.format("%s0x%02x", if (i < 0) "-" else " ", (i&0x7f).asInstanceOf[AnyRef]))
  ).mkString("Array[Byte](", ", ", ")")

  def mkkeys =
      String.format("""
val ALGORITHM_PARAMETERS = new IvParameterSpec(%s)
val A1 = %s
val A2 = %s
val A3 = %s
val B1 = %s
val B2 = %s
val B3 = %s
""", ((1 to 7) map (_ => mkkey)): _*)
*/

/**
 * Carrier of character bytes that securely disposes of itself when requested or when GCd.
 */
public final class SecureString
{
    private static final String KEY_FACTORY_ALGORITHM = "DESede";
    private static final String CIPHER_TRANSFORMATION = "DESede/CBC/PKCS5Padding";

    private static final String ENCODING = "ISO-8859-1";

    static
    {
        Security.addProvider(new com.sun.crypto.provider.SunJCE()); // this is safe if somebody has already added it
    }

    /** Encrypt and base64 encode the given string using the given encryption parameters. This should only be used for non-sensitive data */
    public static String maskString(String clearText,
                                    IvParameterSpec initVector,
                                    byte[] a1, byte[] a2, byte[] a3,
                                    byte[] b1, byte[] b2, byte[] b3) throws GeneralSecurityException
    {
        SecureString ss = new SecureString(clearText.toCharArray());

        String cipherText = new String(Base64.encode(ss.encrypt(initVector, a1, a2, a3, b1, b2, b3)));

        ss.dispose();

        return cipherText;
    }

    /** Base64 decode and decrypt the given string using the given encryption parameters. This should only be used for non-sensitive data */
    public static String unmaskString(String cipherText,
                                      IvParameterSpec initVector,
                                      byte[] a1, byte[] a2, byte[] a3,
                                      byte[] b1, byte[] b2, byte[] b3) throws GeneralSecurityException
    {
        try {
            SecureString ss = SecureString.decrypt(Base64.decode(cipherText.getBytes(ENCODING)),
                                                   initVector, a1, a2, a3, b1, b2, b3);

            String s = new String(ss.getCharacters());
            ss.dispose();

            return s;
        } catch (java.io.UnsupportedEncodingException e) {
            throw new RuntimeException(e.toString());
        }
    }

    /** Encrypt some bytes */
    public static byte[] compactEncryptBytes(byte[] in,
                                             IvParameterSpec initVector,
                                             byte[] a1, byte[] a2, byte[] a3,
                                             byte[] b1, byte[] b2, byte[] b3) throws GeneralSecurityException
    {
        Cipher c = getCipher(Cipher.ENCRYPT_MODE, KEY_FACTORY_ALGORITHM, "DESede/CFB8/NoPadding",
                             initVector, a1, a2, a3, b1, b2, b3);
        return c.doFinal(in);
    }

    /** Decrypt some bytes */
    public static byte[] compactDecryptBytes(byte[] in,
                                             IvParameterSpec initVector,
                                             byte[] a1, byte[] a2, byte[] a3,
                                             byte[] b1, byte[] b2, byte[] b3) throws GeneralSecurityException
    {
        Cipher c = getCipher(Cipher.DECRYPT_MODE, KEY_FACTORY_ALGORITHM, "DESede/CFB8/NoPadding",
                             initVector, a1, a2, a3, b1, b2, b3);
        return c.doFinal(in);
    }

    private char[] characters;

    /**
     * Construct a new SecureString object holding the given character array.
     * <p>
     * <strong>NOTE</strong>: after using this constructor you should null any reference to the char[] array and only access
     * it via the constructed SecureString object.
     */
    public SecureString(char[] ary)
    {
        characters = ary;
    }

    /**
     * Get the characters being held by this object.
     * <p>
     * <strong>NOTE</strong>: do not duplicate the returned array. Try not to constitute the returned array into a
     * String object.
     *
     * @throws IllegalStateException when this SecureString has been disposed
     */
    public synchronized char[] getCharacters()
    {
        if (characters == null) {
            throw new IllegalStateException("SecureString disposed.");
        }

        return characters;
    }

    /**
     * Dispose of this object, clearing the held characters.
     */
    public synchronized void dispose()
    {
        for (int i = 0; i < characters.length; i++) {
            characters[i] = '\uFFFF';
        }

        characters = null;
    }

    /**
     * Finalizer for this object.
     */
    protected void finalize()
    {
        dispose();
    }

    /**
     * Encrypt the contents of this SecureString using Triple-DES.
     * <p>
     * <strong>NOTE: to avoid the clear text characters or bytes being held in memory by a non-clearable object,
     * this method does ASCII-to-byte conversion directly by demoting the clear text char[]s to byte[]s. This
     * implies that you should only ever use valid low-half ASCII characters in values being encrypted/decrypted
     * by this facility.
     *
     * @param initVector IV for the encryption algorithm
     * @param a1 key data
     * @param a2 key data
     * @param a3 key data
     * @param b1 key data
     * @param b2 key data
     * @param b3 key data
     */
    public byte[] encrypt(IvParameterSpec initVector,
                          byte[] a1, byte[] a2, byte[] a3,
                          byte[] b1, byte[] b2, byte[] b3) throws GeneralSecurityException
    {
        Cipher c = getCipher(Cipher.ENCRYPT_MODE, initVector, a1, a2, a3, b1, b2, b3);
        byte[] clearText = new byte[characters.length];

        try {
            for (int i = 0; i < characters.length; i++) {
                clearText[i] = (byte)characters[i];
            }

            return c.doFinal(clearText);
        } finally {
            mangle3(clearText);
        }
    }

    /**
     * Encrypt the contents of this SecureString using Triple-DES.
     * <p>
     * <strong>NOTE: to avoid the clear text characters or bytes being held in memory by a non-clearable object,
     * this method does ASCII-to-byte conversion directly by demoting the clear text char[]s to byte[]s. This
     * implies that you should only ever use valid low-half ASCII characters in values being encrypted/decrypted
     * by this facility.
     *
     * @param secureInfo key data
     */
    public byte[] encrypt(SecureInfo secureInfo) throws GeneralSecurityException
    {
        return encrypt(secureInfo.getIv(), secureInfo.getA1(), secureInfo.getA2(), secureInfo.getA3(), secureInfo.getB1(), secureInfo.getB2(), secureInfo.getB3());
    }

    /**
     * Static factory that creates SecureString instances by decrypting the given byte array using Triple-DES.
     * <p>
     * <strong>NOTE: to avoid the clear text characters or bytes being held in memory by a non-clearable object,
     * this method does byte-to-ASCII conversion directly by promoting the clear text byte[]s to char[]s. This
     * implies that you should only ever use valid low-half ASCII characters in values being encrypted/decrypted
     * by this facility.
     *
     * @param cipherText the bytes to decrypt
     * @param a1 key data
     * @param a2 key data
     * @param a3 key data
     * @param b1 key data
     * @param b2 key data
     * @param b3 key data
     */
    public static SecureString decrypt(byte[] cipherText,
                                       IvParameterSpec initVector,
                                       byte[] a1, byte[] a2, byte[] a3,
                                       byte[] b1, byte[] b2, byte[] b3) throws GeneralSecurityException
    {
        Cipher c = getCipher(Cipher.DECRYPT_MODE, initVector, a1, a2, a3, b1, b2, b3);

        byte[] clearBytes = c.doFinal(cipherText);
        char[] clearChars = new char[clearBytes.length];

        try {
            for (int i = 0; i < clearBytes.length; i++) {
                clearChars[i] = (char)clearBytes[i];
            }

            return new SecureString(clearChars);
        } finally {
            mangle3(clearBytes);
        }
    }

    /**
     * Static factory that creates SecureString instances by decrypting the given byte array using Triple-DES.
     * <p>
     * <strong>NOTE: to avoid the clear text characters or bytes being held in memory by a non-clearable object,
     * this method does byte-to-ASCII conversion directly by promoting the clear text byte[]s to char[]s. This
     * implies that you should only ever use valid low-half ASCII characters in values being encrypted/decrypted
     * by this facility.
     *
     * @param cipherText the bytes to decrypt
     * @param secureInfo key parameters
     */
    public static SecureString decrypt(byte[] cipherText, SecureInfo secureInfo) throws GeneralSecurityException
    {
        return decrypt(cipherText, secureInfo.getIv(), secureInfo.getA1(), secureInfo.getA2(), secureInfo.getA3(), secureInfo.getB1(), secureInfo.getB2(), secureInfo.getB3());
    }

    /** Utility method for obtaining a Cipher for encryption or decryption. */
    private static Cipher getCipher(int mode,
                                    IvParameterSpec iv,
                                    byte[] a1, byte[] a2, byte[] a3,
                                    byte[] b1, byte[] b2, byte[] b3) throws GeneralSecurityException
    {
        return getCipher(mode, KEY_FACTORY_ALGORITHM, CIPHER_TRANSFORMATION, iv, a1, a2, a3, b1, b2, b3);
    }

    /** Utility method for obtaining a Cipher for encryption or decryption. */
    private static Cipher getCipher(int mode,
                                    String algorithm, String transform,
                                    IvParameterSpec iv,
                                    byte[] a1, byte[] a2, byte[] a3,
                                    byte[] b1, byte[] b2, byte[] b3) throws GeneralSecurityException
    {
        byte[] kbytes = mangle2(mangle1(a1, b1),
                                mangle1(a2, b2),
                                mangle1(a3, b3));

        try {
            DESedeKeySpec keySpec = new DESedeKeySpec(kbytes);
            SecretKeyFactory factory = SecretKeyFactory.getInstance(algorithm);
            SecretKey key = factory.generateSecret(keySpec);

            Cipher c = Cipher.getInstance(transform);
            c.init(mode, key, iv);
            return c;
        } finally {
            mangle3(kbytes);
        }
    }

    /**
     * Utility method to compose an 8-byte array from two 8-byte arrays by XORing them against each other.
     * <p>
     * Note: this method is named badly intentionally.
     */
    private static byte[] mangle1(byte[] a, byte[] b)
    {
        byte[] result = new byte[a.length];

        for (int i = 0; i < a.length; i++) {
            result[i] = (byte)(a[i] ^ b[i]);
        }

        return result;
    }

    /**
     * Utility method to construct a whole DES key byte array from three separate ones.
     * <p>
     * Note: this method is named badly intentionally.
     */
    private static byte[] mangle2(byte[] a, byte[] b, byte[] c)
    {
        byte[] result = new byte[a.length + b.length + c.length];

        System.arraycopy(a, 0, result, 0, a.length);
        System.arraycopy(b, 0, result, a.length, b.length);
        System.arraycopy(c, 0, result, a.length + b.length, c.length);

        mangle3(a);
        mangle3(b);
        mangle3(c);

        return result;
    }

    /**
     * Utility method to wipe out an array.
     * <p>
     * Note: this method is named badly intentionally.
     */
    private static void mangle3(byte[] a)
    {
        for (int i = 0; i < a.length; i++) {
            a[i] = (byte)0xff;
        }
    }

    /**
     * Load and parse a config file containing the hex strings of the encryption parameters
     * @param basename Base of filename which contains parameters
     * @return The SecureInfo object used within this file
     * @throws FileNotFoundException
     * @throws IOException
     */
    public static SecureInfo getSecureInfo(File path, String filename) throws FileNotFoundException, IOException {
        File file = new File(path, filename);
        if (!file.exists()) {
            throw new FileNotFoundException("PXS config file does not exist.");
        }

        Properties p = new Properties();
        FileInputStream fileStream = null;
        BufferedInputStream bufferedStream = null;
        try {
            fileStream = new FileInputStream(file);
            bufferedStream = new BufferedInputStream(fileStream);
            p.load(bufferedStream);
        } finally {
            if (bufferedStream != null)  bufferedStream.close();
            if (fileStream != null)  fileStream.close();
        }

        return new SecureInfo(p.getProperty("iv"),
                              p.getProperty("a1"),
                              p.getProperty("a2"),
                              p.getProperty("a3"),
                              p.getProperty("b1"),
                              p.getProperty("b2"),
                              p.getProperty("b3"));
    }

    public static class SecureInfo {
        private IvParameterSpec iv;
        private byte[] a1;
        private byte[] a2;
        private byte[] a3;
        private byte[] b1;
        private byte[] b2;
        private byte[] b3;

        private SecureInfo(String iv, String a1, String a2, String a3, String b1, String b2, String b3) {
            this.iv = new IvParameterSpec(hexToByteArray(iv));
            this.a1 = hexToByteArray(a1);
            this.a2 = hexToByteArray(a2);
            this.a3 = hexToByteArray(a3);
            this.b1 = hexToByteArray(b1);
            this.b2 = hexToByteArray(b2);
            this.b3 = hexToByteArray(b3);
        }

        private byte[] hexToByteArray(String hexDigits) {
            if (hexDigits == null || hexDigits.length() != 16) {
                throw new RuntimeException("Invalid parameter length");
            }
            byte[] bytes = new byte[hexDigits.length() / 2];
            for (int i = 0; i < bytes.length; i++) {
               bytes[i] = (byte) Integer.parseInt(hexDigits.substring(i*2, (i*2)+2), 16);
            }
            return bytes;
        }

        public IvParameterSpec getIv() {
            return iv;
        }
        public byte[] getA1() {
            return a1;
        }
        public byte[] getA2() {
            return a2;
        }
        public byte[] getA3() {
            return a3;
        }
        public byte[] getB1() {
            return b1;
        }
        public byte[] getB2() {
            return b2;
        }
        public byte[] getB3() {
            return b3;
        }
    }
}
