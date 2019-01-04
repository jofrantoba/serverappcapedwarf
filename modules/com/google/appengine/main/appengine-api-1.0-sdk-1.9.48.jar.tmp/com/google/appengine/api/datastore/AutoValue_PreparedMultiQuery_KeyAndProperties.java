
package com.google.appengine.api.datastore;

import java.util.Map;
import javax.annotation.Generated;
import javax.annotation.Nullable;

@Generated("com.google.auto.value.processor.AutoValueProcessor")
 final class AutoValue_PreparedMultiQuery_KeyAndProperties extends PreparedMultiQuery.KeyAndProperties {

  private final Key key;
  private final Map<String, Object> properties;

  AutoValue_PreparedMultiQuery_KeyAndProperties(
      Key key,
      @Nullable Map<String, Object> properties) {
    if (key == null) {
      throw new NullPointerException("Null key");
    }
    this.key = key;
    this.properties = properties;
  }

  @Override
  Key key() {
    return key;
  }

  @Nullable
  @Override
  Map<String, Object> properties() {
    return properties;
  }

  @Override
  public String toString() {
    return "KeyAndProperties{"
        + "key=" + key + ", "
        + "properties=" + properties
        + "}";
  }

  @Override
  public boolean equals(Object o) {
    if (o == this) {
      return true;
    }
    if (o instanceof PreparedMultiQuery.KeyAndProperties) {
      PreparedMultiQuery.KeyAndProperties that = (PreparedMultiQuery.KeyAndProperties) o;
      return (this.key.equals(that.key()))
           && ((this.properties == null) ? (that.properties() == null) : this.properties.equals(that.properties()));
    }
    return false;
  }

  @Override
  public int hashCode() {
    int h = 1;
    h *= 1000003;
    h ^= this.key.hashCode();
    h *= 1000003;
    h ^= (properties == null) ? 0 : this.properties.hashCode();
    return h;
  }

}
