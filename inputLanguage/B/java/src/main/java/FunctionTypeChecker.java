public class FunctionTypeChecker<K, V> {

    private final BRelation<K, V> fun;
    private boolean value = true;

    public FunctionTypeChecker(final BRelation<K, V> fun) {
        this.fun = fun;
    }

    public FunctionTypeChecker<K, V> hasDomain(BSet<K> domain) {
        value &= fun.domain().equals(domain);
        return this;
    }

    public FunctionTypeChecker<K, V> hasRange(BSet<V> range) {
        value &= fun.range().equals(range);
        return this;
    }

    public FunctionTypeChecker<K, V> isFunction() {
        return this;
    }

    public FunctionTypeChecker<K, V> isInjection() {
        return this;
    }

    public FunctionTypeChecker<K, V> isSurjection() {
        return this;
    }

    public FunctionTypeChecker<K, V> isBijection() {
        return this;
    }

    public FunctionTypeChecker<K, V> isTotal(BSet<K> domain) {
        value &= fun.domain().equals(domain);
        return this;
    }

    public FunctionTypeChecker<K, V> isPartial() {
        return this;
    }

    public boolean value() {
        return value;
    }
}