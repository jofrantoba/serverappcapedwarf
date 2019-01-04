/*
 * Copyright (C) 2011 The Guava Authors
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

package com.google.common.collect;

import com.google.common.annotations.GwtCompatible;
import java.util.ArrayDeque;
import java.util.Collection;
import java.util.Deque;
import java.util.PriorityQueue;
import java.util.Queue;

/**
 * Static utility methods pertaining to {@link Queue} and {@link Deque} instances.
 * Also see this class's counterparts {@link Lists}, {@link Sets}, and {@link Maps}.
 *
 * @author Kurt Alfred Kluever
 * @since 11.0
 */
@GwtCompatible(emulated = true)
public final class Queues {
  private Queues() {}

  // ArrayBlockingQueue

  // ArrayDeque

  /**
   * Creates an empty {@code ArrayDeque}.
   *
   * @since 12.0
   */
  public static <E> ArrayDeque<E> newArrayDeque() {
    return new ArrayDeque<E>();
  }

  /**
   * Creates an {@code ArrayDeque} containing the elements of the specified iterable,
   * in the order they are returned by the iterable's iterator.
   *
   * @since 12.0
   */
  public static <E> ArrayDeque<E> newArrayDeque(Iterable<? extends E> elements) {
    if (elements instanceof Collection) {
      return new ArrayDeque<E>(Collections2.cast(elements));
    }
    ArrayDeque<E> deque = new ArrayDeque<E>();
    Iterables.addAll(deque, elements);
    return deque;
  }

  // ConcurrentLinkedQueue

  // LinkedBlockingDeque

  // LinkedBlockingQueue

  // LinkedList: see {@link com.google.common.collect.Lists}

  // PriorityBlockingQueue

  // PriorityQueue

  /**
   * Creates an empty {@code PriorityQueue} with the ordering given by its
   * elements' natural ordering.
   *
   * @since 11.0 (requires that {@code E} be {@code Comparable} since 15.0).
   */
  public static <E extends Comparable> PriorityQueue<E> newPriorityQueue() {
    return new PriorityQueue<E>();
  }

  /**
   * Creates a {@code PriorityQueue} containing the given elements.
   *
   * <b>Note:</b> If the specified iterable is a {@code SortedSet} or a {@code PriorityQueue},
   * this priority queue will be ordered according to the same ordering.
   *
   * @since 11.0 (requires that {@code E} be {@code Comparable} since 15.0).
   */
  public static <E extends Comparable> PriorityQueue<E> newPriorityQueue(
      Iterable<? extends E> elements) {
    if (elements instanceof Collection) {
      return new PriorityQueue<E>(Collections2.cast(elements));
    }
    PriorityQueue<E> queue = new PriorityQueue<E>();
    Iterables.addAll(queue, elements);
    return queue;
  }

  // SynchronousQueue

  /**
   * Returns a synchronized (thread-safe) queue backed by the specified queue. In order to
   * guarantee serial access, it is critical that <b>all</b> access to the backing queue is
   * accomplished through the returned queue.
   *
   * <p>It is imperative that the user manually synchronize on the returned queue when accessing
   * the queue's iterator: <pre>   {@code
   *
   *   Queue<E> queue = Queues.synchronizedQueue(MinMaxPriorityQueue.<E>create());
   *   ...
   *   queue.add(element);  // Needn't be in synchronized block
   *   ...
   *   synchronized (queue) {  // Must synchronize on queue!
   *     Iterator<E> i = queue.iterator(); // Must be in synchronized block
   *     while (i.hasNext()) {
   *       foo(i.next());
   *     }
   *   }}</pre>
   *
   * <p>Failure to follow this advice may result in non-deterministic behavior.
   *
   * <p>The returned queue will be serializable if the specified queue is serializable.
   *
   * @param queue the queue to be wrapped in a synchronized view
   * @return a synchronized view of the specified queue
   * @since 14.0
   */
  public static <E> Queue<E> synchronizedQueue(Queue<E> queue) {
    return Synchronized.queue(queue, null);
  }

  /**
   * Returns a synchronized (thread-safe) deque backed by the specified deque. In order to
   * guarantee serial access, it is critical that <b>all</b> access to the backing deque is
   * accomplished through the returned deque.
   *
   * <p>It is imperative that the user manually synchronize on the returned deque when accessing
   * any of the deque's iterators: <pre>   {@code
   *
   *   Deque<E> deque = Queues.synchronizedDeque(Queues.<E>newArrayDeque());
   *   ...
   *   deque.add(element);  // Needn't be in synchronized block
   *   ...
   *   synchronized (deque) {  // Must synchronize on deque!
   *     Iterator<E> i = deque.iterator(); // Must be in synchronized block
   *     while (i.hasNext()) {
   *       foo(i.next());
   *     }
   *   }}</pre>
   *
   * <p>Failure to follow this advice may result in non-deterministic behavior.
   *
   * <p>The returned deque will be serializable if the specified deque is serializable.
   *
   * @param deque the deque to be wrapped in a synchronized view
   * @return a synchronized view of the specified deque
   * @since 15.0
   */
  public static <E> Deque<E> synchronizedDeque(Deque<E> deque) {
    return Synchronized.deque(deque, null);
  }
}

