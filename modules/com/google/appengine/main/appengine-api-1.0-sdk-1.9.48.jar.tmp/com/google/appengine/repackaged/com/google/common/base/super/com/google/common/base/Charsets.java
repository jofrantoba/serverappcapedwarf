/*
 * Copyright (C) 2007 The Guava Authors
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
 * in compliance with the License. You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License
 * is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
 * or implied. See the License for the specific language governing permissions and limitations under
 * the License.
 */

package com.google.common.base;

import com.google.common.annotations.GwtCompatible;
import java.nio.charset.Charset;

/**
 * Contains constant definitions for the six standard {@link Charset} instances, which are
 * guaranteed to be supported by all Java platform implementations.
 *
 * <p>Assuming you're free to choose, note that <b>{@link #UTF_8} is widely preferred</b>.
 *
 * <p>See the Guava User Guide article on <a
 * href="https://github.com/google/guava/wiki/StringsExplained#charsets">{@code Charsets}</a>.
 *
 * @author Mike Bostock
 * @since 1.0
 */
@GwtCompatible(emulated = true)
public final class Charsets {
  private Charsets() {}

  /**
   * ISO-8859-1: ISO Latin Alphabet Number 1 (ISO-LATIN-1).
   *
   * <p><b>Note for Java 7 and later:</b> this constant should be treated as deprecated; use {@link
   * java.nio.charset.StandardCharsets#ISO_8859_1} instead.
   *
   * <!-- MOE:begin_intracomment_strip -->
   * @deprecated Use {@link java.nio.charset.StandardCharsets#ISO_8859_1} instead.
   * <!-- MOE:end_intracomment_strip -->
   */
  @Deprecated // MOE:strip_line
  public static final Charset ISO_8859_1 = Charset.forName("ISO-8859-1");

  /**
   * UTF-8: eight-bit UCS Transformation Format.
   *
   * <p><b>Note for Java 7 and later:</b> this constant should be treated as deprecated; use {@link
   * java.nio.charset.StandardCharsets#UTF_8} instead.
   *
   * <!-- MOE:begin_intracomment_strip -->
   * @deprecated Use {@link java.nio.charset.StandardCharsets#UTF_8} instead.
   * <!-- MOE:end_intracomment_strip -->
   */
  @Deprecated // MOE:strip_line
  public static final Charset UTF_8 = Charset.forName("UTF-8");

  /*
   * Please do not add new Charset references to this class, unless those character encodings are
   * part of the set required to be supported by all Java platform implementations! Any Charsets
   * initialized here may cause unexpected delays when this class is loaded. See the Charset
   * Javadocs for the list of built-in character encodings.
   */
}

