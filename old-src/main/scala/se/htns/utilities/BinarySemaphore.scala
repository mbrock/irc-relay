package se.htns.utilities


import concurrent.Lock
import java.util.concurrent.Semaphore

class BinarySemaphore {
  private val lock : Lock = new Lock

  def activate = lock.release
  def block = lock.acquire
}