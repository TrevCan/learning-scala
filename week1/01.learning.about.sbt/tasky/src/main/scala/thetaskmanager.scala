import java.time.LocalDate

object TaskManager {

//  def getAllTasks(task_list: Seq[Task]): Seq[Task] = task_list.filter( t => true )
//
//  def getTasksBeforeToday(task_list: Seq[Task]): Seq[Task] = task_list.filter( t => LocalDate.now().isAfter( t.dueOn ) )
//
//  def getTasksAfterToday(task_list: Seq[Task]): Seq[Task] = task_list.filter( t => LocalDate.now().isBefore( t.dueOn ) )
//
//  def isAfter(task: Task, task_v: Task ): Boolean = task.dueOn.isAfter( task_v.dueOn )
//
//  def toString( tasks: Seq[Task] ): String = tasks.mkString ( sep: "iifaofao" )
  // def toString( tasks: Seq[Task] ): String = tasks.foreach( sT => sT.title ).toString 

  // def toString( tasks: Seq[Task] ): String = "aifjiafajofoaf"

  // ( for ( single_task <- tasks ) single_task.title
  //

  // this function just returns nothing or null
  // def allTasksDueToday( taks: List[Task]): List[Task] = Nil
  //

  // this one actually filters the task list 
  def allTasksDueToday( tasks: List[Task]): List[Task] = tasks.filter( p => LocalDate.now().isEqual( p.dueOn ) )



}
