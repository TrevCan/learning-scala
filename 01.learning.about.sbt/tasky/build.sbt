name := "tasky"
version := "0.1.0"
//libraryDependencies += "org.scalatest" % "scalatest_2.12.15" % "2.2.6" % "test"
//libraryDependencies += "org.scalatest" %% "scalatest_2.12" % "test"
//libraryDependencies += "org.scalatest" % "scalatest_2.12" % "3.0.0" % "test"

//libraryDependencies += "org.scalatest" %% "scalatest" % "test"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.0" % "test"

// must define the task as a taskKey with a description.
val gitCommitCountTask = taskKey[String]("Prints commit count of current branch.")

// this task is the equivalent of this in your shell:
// 		git rev-list --count $(git branch | awk -F ' ' '{ print $2 } ' | tr -d '\n' )

// it gets the git branch through a different method, with the actual
// branch command in git
gitCommitCountTask := {

	//val branch = scala.sys.process.Process("git symbolic-ref -q HEAD").lines.head.replace("refs/heads/","")
	val branch = scala.sys.process.Process("git branch").lines.head.replace("* ","")
	val commitCount = scala.sys.process.Process(s"git rev-list --count $branch").lines.head
	println(s"total number of commits on [$branch]: $commitCount")
	commitCount



}

