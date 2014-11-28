package util

import models.Iine
import models.Iines
import models.Comments
import models.Subjects
import models.Tasks

object Demo {

  def save {
    val iines = Iines.all()
    val comments = Comments.all()
    val subjects = Subjects.all()
    val tasks = Tasks.all()
  }

}