#!/usr/bin/python

# Usage example:
# python comment_threads.py --channelid='<channel_id>' --videoid='<video_id>' --text='<text>'

import httplib2
import os
import sys
import time
import traceback


from apiclient.discovery import build_from_document
from apiclient.errors import HttpError
from oauth2client.client import flow_from_clientsecrets
from oauth2client.file import Storage
from oauth2client.tools import argparser, run_flow


# The CLIENT_SECRETS_FILE variable specifies the name of a file that contains

# the OAuth 2.0 information for this application, including its client_id and
# client_secret. You can acquire an OAuth 2.0 client ID and client secret from
# the {{ Google Cloud Console }} at
# {{ https://cloud.google.com/console }}.
# Please ensure that you have enabled the YouTube Data API for your project.
# For more information about using OAuth2 to access the YouTube Data API, see:
#   https://developers.google.com/youtube/v3/guides/authentication
# For more information about the client_secrets.json file format, see:
#   https://developers.google.com/api-client-library/python/guide/aaa_client_secrets
CLIENT_SECRETS_FILE = "client_secrets.json"

# This OAuth 2.0 access scope allows for full read/write access to the
# authenticated user's account and requires requests to use an SSL connection.
YOUTUBE_READ_WRITE_SSL_SCOPE = "https://www.googleapis.com/auth/youtube.force-ssl"
YOUTUBE_API_SERVICE_NAME = "youtube"
YOUTUBE_API_VERSION = "v3"

# This variable defines a message to display if the CLIENT_SECRETS_FILE is
# missing.
MISSING_CLIENT_SECRETS_MESSAGE = """
WARNING: Please configure OAuth 2.0

To make this sample run you will need to populate the client_secrets.json file
found at:
   %s
with information from the APIs Console
https://console.developers.google.com

For more information about the client_secrets.json file format, please visit:
https://developers.google.com/api-client-library/python/guide/aaa_client_secrets
""" % os.path.abspath(os.path.join(os.path.dirname(__file__),
                                   CLIENT_SECRETS_FILE))

# Authorize the request and store authorization credentials.
def get_authenticated_service(args):
  flow = flow_from_clientsecrets(CLIENT_SECRETS_FILE, scope=YOUTUBE_READ_WRITE_SSL_SCOPE,
    message=MISSING_CLIENT_SECRETS_MESSAGE)

  storage = Storage("%s-oauth2.json" % sys.argv[0])
  credentials = storage.get()

  if credentials is None or credentials.invalid:
    credentials = run_flow(flow, storage, args)

  # Trusted testers can download this discovery document from the developers page
  # and it should be in the same directory with the code.
  with open("youtube-v3-discoverydocument.json", "r") as f:
    doc = f.read()
    return build_from_document(doc, http=credentials.authorize(httplib2.Http()))


def nested_get(obj, path):
    return reduce(lambda obj, attr: obj.__getitem__(attr), path.split("."), obj)

def resource_fmt(resource, fmt):
    return tuple((nested_get(resource, fmt_i) for fmt_i in fmt))

def resource_fmt_default(resource):
    return resource_fmt(resource, DEFAULT_FMTS[resource["kind"]])

DEFAULT_FMTS={
    "youtube#subscription": ("kind", "snippet.title", "snippet.resourceId.channelId"),
    "youtube#commentThread": ("kind", "id", "snippet.topLevelComment.snippet.authorDisplayName", "snippet.topLevelComment.snippet.textOriginal"),
}

# Call the API's commentThreads.list method to list the existing comments.
def depaginate(client, resource_name, per_page=100, verbose=False,
               max_results=None,
               part="snippet",
               **kwargs):

    "providing the resource name instead of the object allows us to cache api results locally"

    resource=client.__getattribute__(resource_name).__call__()

    page_token=None
    page_idx=1

    total_results=None
    per_page=None

    cnt=0

    while True:
        results = resource.list(
            pageToken=page_token,
            part=part,
            maxResults=50,
            **kwargs
        ).execute()


        page_token=results.get("nextPageToken")
        
        if total_results==None:
            total_results=results["pageInfo"]["totalResults"]
            per_page=results["pageInfo"]["resultsPerPage"]
            print ("total results: {}".format(total_results))

        # items.extend(results["items"])
        for item in results["items"]:
            yield item
            cnt+=1

        if page_token==None:
            # import pdb;pdb.set_trace()
            print (cnt, total_results)
            # assert(len(items)==total_results)
            break

        # if max_results != None and page_no*per_page>=max_results:
        #     break

        if verbose:
            sys.stdout.write("\ron page {}/{}".format(page_idx, total_results/per_page))
            sys.stdout.flush()

        page_idx+=1

def get_comments(client,
                 video_id=None,
                 channel_id=None,
                 search_terms=None,
                 comment_id=None):

    filter_args={"videoId":video_id,
                 "allThreadsRelatedToChannelId":channel_id,
                 "searchTerms":search_terms,
                 "id":comment_id}

    # print("{} comments for video {}".format(len(items), filter_args))
    items=depaginate(client,
                     "commentThreads",
                     textFormat="plainText",
                     **filter_args)

    return items

def get_subscriptions(client, channel_id=None, mine=None):
    return depaginate(client, "subscriptions", mine=mine, channelId=channel_id)


def remove_empty_kwargs(**kwargs):
    good_kwargs = {}
    if kwargs is not None:
        for key, value in kwargs.items():
            if value:
                good_kwargs[key] = value
    return good_kwargs

# Sample python code for comments.delete
def comments_delete(client, **kwargs):
  # See full sample for function
  kwargs = remove_empty_kwargs(**kwargs)

  response = client.comments().delete(
    **kwargs
  ).execute()

  print (response)
  return response

if __name__ == "__main__":
  # The "channelid" option specifies the YouTube channel ID that uniquely
  # identifies the channel for which the comment will be inserted.
  argparser.add_argument("--channelid",
    help="Required; ID for channel for which the comment will be inserted.")
  # The "videoid" option specifies the YouTube video ID that uniquely
  # identifies the video for which the comment will be inserted.
  argparser.add_argument("--videoid",
    help="Required; ID for video for which the comment will be inserted.")
  # The "text" option specifies the text that will be used as comment.
  argparser.add_argument("--text", help="Required; text that will be used as comment.")
  args = argparser.parse_args()

  args.videoid="***REMOVED***"

  if not args.videoid:
    exit("Please specify videoid using the --videoid= parameter.")

  client = get_authenticated_service(args)
  channel_id="***REMOVED***"

  comments = get_comments(client, channel_id=channel_id)
  for comment in comments:
      print (resource_fmt_default(comment))

  # print_comments(comments)

  # for sub in get_subscriptions(client, mine=True):
  #     print (resource_fmt_default(sub))
  # print ([comment])

  for (i, sub) in enumerate(get_subscriptions(client, channel_id=channel_id)):
      print (resource_fmt_default(sub))
      chan=nested_get(sub, "snippet.resourceId.channelId")
      for comment in get_comments(client, channel_id=chan,
                                  search_terms="***REMOVED***"):
          try:
              video_id=nested_get(comment, "snippet.videoId")
              url="https://www.youtube.com/watch?v={}".format(video_id)
          except:
              url=None
          try:
              print ("\t{} ({})".format("\t".join(resource_fmt_default(comment)), url))
          except:
              traceback.print_exc()
              import pdb;pdb.set_trace()


