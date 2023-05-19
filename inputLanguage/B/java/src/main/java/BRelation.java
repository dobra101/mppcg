import org.jetbrains.annotations.NotNull;

import java.util.*;
import java.util.stream.Collectors;

@SuppressWarnings("unchecked")
public class BRelation<K extends Comparable<K>, V extends Comparable<V>> implements Set<BCouple<K, V>>, Comparable<BRelation<Integer, Integer>> {
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

    public List<Object> image(BInterval interval) {
        List<Object> result = new ArrayList<>();
        for (BCouple<K, V> entry : entries) {
            if (entry.left instanceof Integer && interval.contains((Integer) entry.left)) {
                result.add(entry.right);
            }
        }
        return result;
    }

    public BRelation<V, K> reverse() {
        List<BCouple<V, K>> reversed = entries.stream().map(c -> new BCouple<V, K>(c.right, c.left)).toList();
        return new BRelation<>(reversed);
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

    public BRelation<K, V> overwrite(BRelation<K, V> other) {
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

    @Override
    public int compareTo(@NotNull final BRelation<Integer, Integer> o) {
        return 0;
    }
}