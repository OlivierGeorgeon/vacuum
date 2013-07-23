package utils ;

import java.util.Map ;

public class Entry<K , V> implements Map.Entry<K , V> {

	private final K key ;
	private V value ;

	public Entry( K key , V value ) {
		this.key = key ;
		this.value = value ;
	}

	@Override
	public final K getKey() {
		return this.key ;
	}

	@Override
	public final V getValue() {
		return this.value ;
	}

	@Override
	public final V setValue( V newValue ) {
		V oldValue = this.value ;
		this.value = newValue ;
		return oldValue ;
	}

	@Override
	public final boolean equals( Object o ) {
		if ( ! ( o instanceof Map.Entry ) )
			return false ;
		Map.Entry e = (Map.Entry) o ;
		Object k1 = getKey() ;
		Object k2 = e.getKey() ;
		if ( k1 == k2 || ( k1 != null && k1.equals( k2 ) ) ) {
			Object v1 = getValue() ;
			Object v2 = e.getValue() ;
			if ( v1 == v2 || ( v1 != null && v1.equals( v2 ) ) )
				return true ;
		}
		return false ;
	}

	@Override
	public final int hashCode() {
		return ( this.key == null ? 0 : this.key.hashCode() ) ^
				( this.value == null ? 0 : this.value.hashCode() ) ;
	}

	@Override
	public final String toString() {
		return getKey() + "=" + getValue() ;
	}
}
