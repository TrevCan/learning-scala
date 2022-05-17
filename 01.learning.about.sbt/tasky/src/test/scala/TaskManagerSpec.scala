import org.scalatest._
import java.time.LocalDate


class TaskManagerSpec extends FlatSpec with Matchers {

  "An empty task list" should "have 0 tasks due today" in {
    val tasksDueToday = TaskManager.allTasksDueToday(List() )
    tasksDueToday should have length 0

  }

  "A task list with one task due today" should "have 1 task due today" in {
    val task1 = Task("Write on the blog.", LocalDate.now(), Seq("blogging"))
    val task2 = Task("Write on how to use linux", LocalDate.now().plusDays(1), Seq("linux"))
    val tasksDueToday = TaskManager.allTasksDueToday(List(task1,task2))
    tasksDueToday should have length 1 

  }

}
