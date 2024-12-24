unit uDemo08;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.StdCtrls
  , uJX3Number
  , uJX3Boolean
  , uJX3String
  , uJX3Object
  , uJX3List
  , uJX3Dictionary
  ;

type

  TForm4 = class(TForm)
    Memo1: TMemo;
    Button: TButton;
    procedure ButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  TUser = class(TJX3Object)
    name: TJX3Str;
    email: TJX3Str;
    login: TJX3Str;
    avatar_url: TJX3Str;
    gravatar_id: TJX3Str;
    id: TJX3Num;
    url: TJX3Str;
    html_url: TJX3Str;
    followers_url: TJX3Str;
    following_url: TJX3Str;
    gists_url: TJX3Str;
    starred_url: TJX3Str;
    subscriptions_url: TJX3Str;
    organizations_url: TJX3Str;
    repos_url: TJX3Str;
    events_url: TJX3Str;
    received_events_url: TJX3Str;
    &type: TJX3Str;
    site_admin: TJX3Bool;
  end;

  TOrg = class(TJX3Object)
    id: TJX3Num;
    login: TJX3Str;
    gravatar_id: TJX3Str;
    url: TJX3Str;
    avatar_url: TJX3Str;
  end;

  TAsset =  class(TJX3Object)
    url: TJX3Str;
    id: TJX3Num;
    name : TJX3Str;
    &label: TJX3Str;
    uploader: TUser;
    content_type: TJX3Str;
    state: TJX3Str;
    size: TJX3Num;
    download_count: TJX3Num;
    created_at: TJX3Str;
    updated_at: TJX3Str;
    browser_download_url: TJX3Str;
  end;

  TRelease = class(TJX3Object)
    url: TJX3Str;
    assets_url: TJX3Str;
    upload_url: TJX3Str;
    html_url: TJX3Str;
    id: TJX3Num;
    tag_name: TJX3Str;
    target_commitish: TJX3Str;
    name: TJX3Str;
    draft: TJX3Bool;
    author: TUser;
    prerelease: TJX3Bool;
    created_at: TJX3Str;
    published_at: TJX3Str;
    assets: TJX3List<TAsset>;
    tarball_url: TJX3Str;
    zipball_url: TJX3Str;
    body: TJX3Str;
  end;

  TCommit = class(TJX3Object)
    sha: TJX3Str;
    author: TUser;
    &message: TJX3Str;
    distinct: TJX3Bool;
    url: TJX3Str;
  end;

  TRepo = class(TJX3Object)
    id: TJX3Num;
    name: TJX3Str;
    full_name: TJX3Str;
    owner: TUser;
    url: TJX3Str;
    &private: TJX3Bool;
    html_url: TJX3Str;
    description: TJX3Str;
    fork: TJX3Bool;
    keys_url:  TJX3Str;
    forks_url:  TJX3Str;
    teams_url: TJX3STR;
    hooks_url: TJX3STR;
    issue_events_url: TJX3STR;
    events_url: TJX3STR;
    assignees_url: TJX3STR;
    branches_url: TJX3STR;
    tags_url: TJX3STR;
    blobs_url: TJX3STR;
    git_tags_url: TJX3STR;
    git_refs_url: TJX3STR;
    trees_url: TJX3STR;
    statuses_url: TJX3STR;
    languages_url: TJX3STR;
    stargazers_url: TJX3STR;
    contributors_url: TJX3STR;
    subscribers_url: TJX3STR;
    subscription_url: TJX3STR;
    commits_url: TJX3STR;
    git_commits_url: TJX3STR;
    comments_url: TJX3STR;
    issue_comment_url: TJX3STR;
    contents_url: TJX3STR;
    compare_url: TJX3STR;
    collaborators_url: TJX3Str;
    merges_url: TJX3STR;
    archive_url: TJX3STR;
    downloads_url: TJX3STR;
    issues_url: TJX3STR;
    pulls_url: TJX3STR;
    milestones_url: TJX3STR;
    notifications_url: TJX3STR;
    labels_url: TJX3STR;
    releases_url: TJX3STR;
    created_at: TJX3STR;
    updated_at: TJX3STR;
    pushed_at: TJX3STR;
    git_url: TJX3STR;
    ssh_url: TJX3STR;
    clone_url: TJX3STR;
    svn_url: TJX3STR;
    homepage: TJX3STR;
    size: TJX3Num;
    stargazers_count: TJX3STR;
    watchers_count: TJX3STR;
    language: TJX3STR;
    has_issues: TJX3Bool;
    has_downloads: TJX3Bool;
    has_wiki: TJX3Bool;
    has_pages: TJX3Bool;
    forks_count: TJX3Num;
    mirror_url: TJX3Str;
    open_issues_count: TJX3Num;
    forks: TJX3Num;
    open_issues: TJX3Num;
    watchers: TJX3Num;
    default_branch: TJX3Str;
    &public: TJX3Bool;
  end;

  THead = class(TJX3Object)
    &label: TJX3Str;
    ref: TJX3Str;
    sha: TJX3Str;
    user: TUser;
    repo: TRepo;
  end;

  TLinks = class(TJX3Object)
    href: TJX3Str;
  end;

  TPull_Request = class(TJX3Object)
    id: TJX3Num;
    url: TJX3Str;
    html_url: TJX3Str;
    diff_url: TJX3Str;
    patch_url: TJX3Str;
    issue_url: TJX3Str;
    state: TJX3Str;
    number: TJX3Num;
    locked: TJX3Bool;
    title: TJX3Str;
    user: TUser;
    body: TJX3Str;
    created_at: TJX3Str;
    updated_at: TJX3Str;
    closed_at: TJX3Str;
    merged_at: TJX3Str;
    merge_commit_sha: TJX3Str;
    assignee: TJX3Str;
    milestone: TJX3Str;
    commits_url: TJX3Str;
    review_comments_url: TJX3Str;
    review_comment_url: TJX3Str;
    comments_url: TJX3Str;
    statuses_url: TJX3Str;
    head: THead;
    base: THead;
    [JX3Name('_links')]
    links: TJX3Dic<TLinks>;
    merged: TJX3Bool;
    mergeable: TJX3Bool;
    mergeable_state: TJX3Str;
    merged_by: TJX3Str;
    comments: TJX3Num;
    review_comments: TJX3Num;
    commits: TJX3Num;
    additions: TJX3Num;
    deletions: TJX3Num;
    changed_files: TJX3Num;
  end;

  TIssue = class(TJX3Object)
    url: TJX3Str;
    labels_url: TJX3Str;
    comments_url: TJX3Str;
    events_url: TJX3Str;
    html_url: TJX3Str;
    id: TJX3Num;
    number: TJX3Num;
    title: TJX3Str;
    user: TUser;
    labels: TJX3List<TJX3Str>;
    state: TJX3Str;
    locked: TJX3Bool;
    assignee: TJX3Str;
    milestone: TJX3Str;
    comments: TJX3Num;
    created_at: TJX3Str;
    updated_at: TJX3Str;
    closed_at: TJX3Str;
    pull_request: TPull_Request;
    body: TJX3Str;
  end;

  TPage = class(TJX3Object)
    page_name: TJX3Str;
    title: TJX3Str;
    summary: TJX3Str;
    action: TJX3Str;
    sha: TJX3Str;
    html_url: TJX3Str;
  end;

  TPayload = class(TJX3Object)
    ref: TJX3Str;
    ref_type: TJX3Str;
    number: TJX3Num;
    action: TJX3Str;
    master_branch: TJX3Str;
    description: TJX3Str;
    pusher_type: TJX3Str;
    push_id: TJX3Num;
    size: TJX3Num;
    distinct_size: TJX3Num;
    head: TJX3Str;
    before: TJX3Str;
    commits: TJX3List<TCommit>;
    release: TRelease;
    pull_request: TPull_Request;
    issue: TIssue;
    forkee: TRepo;
    pages: TJX3List<TPage>;
    comment: TJX3Str;
    member: TUser;
  end;

  TElement = class(TJX3Object)
    id: TJX3Str;
    &type: TJX3Str;
    actor: TUser;
    repo: TRepo;
    payload: TPayload;
    &public: TJX3Bool;
    created_at: TJX3Str;
    org: TOrg;
  end;

  TGitHubExtract = class(TJX3Object)
    GitHub:  TJX3List<TElement>
  end;


var
  Form4: TForm4;

implementation
uses
    System.Diagnostics
  ;

{$R *.fmx}

procedure TForm4.ButtonClick(Sender: TObject);
  var
  LGitHubExtract: TGitHubExtract;
  LStream : TStringStream;
  LJsonStr : string;
  LWatch : TStopWatch;
  KB, Ms: Integer;
  LJSize: Int64;
begin
  Memo1.Lines.Clear;

  LWatch := TStopWatch.StartNew;
  Memo1.Lines.add( 'Loading ebay''s Aspects json file :' );
  LJSize := TJX3Object.LoadFromFile('GitHubExtract.json', LJsonStr, TEncoding.UTF8);
  Memo1.Lines.add( Format( '  Stream size: %n KB', [ (LJSize / 1024) ] ));
  Memo1.Lines.add(Format('==> %d ms', [ LWatch.ElapsedMilliseconds ]));

  Memo1.Lines.add( '' );
  Memo1.Lines.add( 'Convert Json String to JSX3 Objects (Deserialize):' );
  LWatch := TStopWatch.StartNew;
  LGitHubExtract := TJX3Object.FromJSON<TGitHubExtract>(LJsonStr, [ joStats, joRaiseException] );
  Memo1.Lines.add(Format('==> %d ms', [ LWatch.ElapsedMilliseconds ]));
  Memo1.Lines.add(Format('==> %n KB/s', [(LJSize / 1024) / (LWatch.ElapsedMilliseconds / 1000)]));

  Memo1.Lines.add( '' );
  Memo1.Lines.Add(Format('Projects: %d', [LGitHubExtract.GitHub.Count]));

  LGitHubExtract.Free;

end;

end.
