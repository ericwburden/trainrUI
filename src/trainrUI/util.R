get_exercise_listing <- function(dir = getwd()) {
  readRDS(paste0(dir, "/.exercises"))
}

add_descriptions <- function(ex_list) {
  ex_list[['text']] <- sapply(
    ex_list[['path']],
    function(path) {
      lines <- readLines(path)
      comment_lines <- lines[grepl("^#.*$", lines)]
      stripped_hashes <- gsub(r"(^#\s+-*\s*)", "", comment_lines)
      paste(stripped_hashes[1:4], collapse = "\n")
    }
  )
  ex_list
}

prettify_labels <- function(labels, colon_after = 2) {
  split_labels <- strsplit(labels, "-")
  strip_dot_r <- lapply(split_labels, function(x) gsub("\\.R$", "", x))
  title_labels <- lapply(strip_dot_r, tools::toTitleCase)
  colon_added <- lapply(title_labels, append, ":", colon_after)
  collapsed <- lapply(colon_added, paste, collapse = " ")
  gsub(" :", ":", collapsed)
}

tree_list <- function(ex_list) {
  pretty_df <- within(ex_list, {
    chapter <- prettify_labels(chapter)
    lesson <- prettify_labels(lesson)
    exercise <- prettify_labels(exercise, 1)
  })

  out_list <- list()

  # For each chapter
  chapters <- unique(pretty_df[['chapter']])
  for (chapter in chapters) {
    chapter_df <- pretty_df[pretty_df$chapter == chapter,]
    chapter_icon <- if (all(chapter_df$completed)) {
      "star"
    } else if (any(chapter_df$completed)) {
      "star-half-alt"
    } else {
      "times"
    }
    out_list[[chapter]] <- structure(list(), sticon = chapter_icon)

    # For each lesson in chapter
    lessons <- chapter_df$lesson
    for (lesson in lessons) {
      lesson_df <- chapter_df[chapter_df$lesson == lesson,]
      lesson_icon <- if (all(lesson_df$completed)) {
        "star"
      } else if (any(lesson_df$completed)) {
        "star-half-alt"
      } else {
        "times"
      }
      out_list[[chapter]][[lesson]] <- structure(list(), sticon = lesson_icon)

      # For each exercise in lesson
      exercises <- lesson_df$exercise
      for (exercise in exercises) {
        exercise_row <- lesson_df[lesson_df$exercise == exercise,]
        exercise_icon <- if (all(exercise_row$completed)) "star" else "times"
        out_list[[chapter]][[lesson]][[exercise_row$exercise]] <- structure(
          exercise_row$text, sticon = exercise_icon
        )
      }
    }
  }
  out_list
}

