#!/bin/bash
#Course Recommender System --

echo "Running Scala ----> "
export PATH=/Users/suchit/neha/java/workspace/CourseRecommender/resources/scala-2.9.3/bin:$PATH
scalac -cp ../resources/scala-2.9.3/lib -d scalaClasses/ *.scala
scala -cp scalaClasses/ CourseRecom
