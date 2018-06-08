def update_comment(youtube, comment):
  comment["snippet"]["topLevelComment"]["snippet"]["textOriginal"] = 'updated'
  update_result = youtube.commentThreads().update(
    part="snippet",
    body=comment
  ).execute()

  comment = update_result["snippet"]["topLevelComment"]
  author = comment["snippet"]["authorDisplayName"]
  text = comment["snippet"]["textDisplay"]
  print "Updated comment for %s: %s" % (author, text)
