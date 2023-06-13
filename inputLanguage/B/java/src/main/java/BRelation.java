import org.jetbrains.annotations.NotNull;

import java.util.*;
import java.util.stream.Collectors;

public class BRelation<K, V> implements Set<BCouple<K, V>> {
    final List<BCouple<K, V>> entries;

    public BRelation(List<BCouple<K, V>> entries) {
        this.entries = entries;
    }

    @SafeVarargs
    public BRelation(BCouple<K, V>... entries) {
        this.entries = new ArrayList<>();
        this.entries.addAll(Arrays.asList(entries));
    }

    public FunctionTypeChecker<K, V> typeCheck() {
        return new FunctionTypeChecker<>(this);
    }

    public BSet<V> image(BSet<K> set) {
        return new BSet<>(set.stream().map(this::get).collect(Collectors.toSet()));
    }

    public BSet<V> image(BInterval interval) {
        Set<V> result = new HashSet<>();
        for (BCouple<K, V> entry : entries) {
            if (entry.left instanceof Integer && interval.contains((Integer) entry.left)) {
                result.add(entry.right);
            }
        }
        return new BSet<>(result);
    }

    public BRelation<V, K> reverse() {
        List<BCouple<V, K>> reversed = entries.stream().map(c -> new BCouple<V, K>(c.right, c.left)).toList();
        return new BRelation<>(reversed);
    }

    public BRelation<V, K> inverse() {
        List<BCouple<V, K>> inversed = entries.stream().map(c -> new BCouple<V, K>(c.right, c.left)).toList();
        return new BRelation<>(inversed);
    }

    public BRelation<K, V> union(BRelation<K, V> other) {
        List<BCouple<K, V>> newEntries = new ArrayList<>(entries);
        newEntries.addAll(other.entries);
        return new BRelation<>(newEntries);
    }

    public BSet<K> domain() {
        return new BSet<>(
                entries.stream()
                        .map(c -> c.left)
                        .collect(Collectors.toSet())
        );
    }

    public BSet<V> range() {
        return new BSet<>(
                entries.stream()
                        .map(c -> c.right)
                        .collect(Collectors.toSet())
        );
    }

    public BRelation<K, V> override(BRelation<K, V> other) {
        List<BCouple<K, V>> result = new ArrayList<>(other.entries);
        result.addAll(domainSubtraction(other.domain()));
        return new BRelation<>(result);
    }

    public BRelation<K, V> domainRestriction(BSet<K> restriction) {
        List<BCouple<K, V>> restricted = entries.stream()
                .filter(couple -> restriction.contains(couple.left))
                .toList();
        return new BRelation<>(restricted);
    }

    public BRelation<K, V> domainSubtraction(BSet<K> restriction) {
        List<BCouple<K, V>> restricted = entries.stream()
                .filter(couple -> !restriction.contains(couple.left))
                .toList();
        return new BRelation<>(restricted);
    }

    public BRelation<K, V> rangeRestriction(BSet<V> restriction) {
        List<BCouple<K, V>> restricted = entries.stream()
                .filter(couple -> restriction.contains(couple.right))
                .toList();
        return new BRelation<>(restricted);
    }

    public BRelation<K, V> rangeSubtraction(BSet<V> restriction) {
        List<BCouple<K, V>> restricted = entries.stream()
                .filter(couple -> !restriction.contains(couple.right))
                .toList();
        return new BRelation<>(restricted);
    }

    public int card() {
        return entries.size();
    }

    public BRelation<K, V> forwardComposition(BRelation<K, V> other) {
        List<BCouple<K, V>> composition = new ArrayList<>();
        for (BCouple<K, V> entry : entries) {
            List<BCouple<K, V>> list = other.entries.stream()
                    .filter(c -> c.left == entry.right)
                    .map(c -> new BCouple<K, V>(entry.left, c.right))
                    .toList();
            composition.addAll(list);
        }
        return new BRelation<K, V>(composition);
    }

    public V get(K key) {
        for (BCouple<K, V> entry : entries) {
            if (entry.left == key) {
                return entry.right;
            }
        }
        return null;
    }

    public BSet<BRelation<K, V>> pow() {
        BSet<BCouple<K, V>> bSet = new BSet<>(new HashSet<>(entries));
        Set<BRelation<K, V>> setOfRelations = bSet.pow().stream()
                .map(subset -> new BRelation<>(subset.stream().toList()))
                .collect(Collectors.toSet());

        return new BSet<>(setOfRelations);
    }

    public boolean add(K key, V value) {
        return add(new BCouple<>(key, value));
    }

    @Override
    public int size() {
        return entries.size();
    }

    @Override
    public boolean isEmpty() {
        return entries.isEmpty();
    }

    @Override
    public boolean contains(final Object o) {
        return entries.contains(o);
    }

    @NotNull
    @Override
    public Iterator<BCouple<K, V>> iterator() {
        return entries.iterator();
    }

    @NotNull
    @Override
    public Object[] toArray() {
        return entries.toArray();
    }

    @NotNull
    @Override
    public <T> T[] toArray(@NotNull final T[] a) {
        return entries.toArray(a);
    }

    @Override
    public boolean add(final BCouple<K, V> c) {
        return entries.add(c);
    }

    @Override
    public boolean remove(final Object key) {
        return entries.remove(key);
    }

    @Override
    public boolean containsAll(@NotNull final Collection<?> c) {
        return false;
    }

    @Override
    public boolean addAll(@NotNull final Collection<? extends BCouple<K, V>> c) {
        return false;
    }

    @Override
    public boolean retainAll(@NotNull final Collection<?> c) {
        return false;
    }

    @Override
    public boolean removeAll(@NotNull final Collection<?> c) {
        return false;
    }

    @Override
    public void clear() {
        entries.clear();
    }

    @Override
    public boolean equals(final Object o) {
        if (!(o instanceof BRelation<?, ?>)) {
            return false;
        }
        return entries.equals(((BRelation<?, ?>) o).entries);
    }

    @Override
    public int hashCode() {
        return entries.hashCode();
    }
}