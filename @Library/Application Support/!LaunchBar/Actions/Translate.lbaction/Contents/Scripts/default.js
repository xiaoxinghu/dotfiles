function runWithString(argument)
{
  const url = 'https://translate.google.cn/#auto/zh-CN/'
  if (LaunchBar.options.commandKey) {
    url = 'https://translate.google.cn/#auto/en/'
  }
  LaunchBar.openURL(url + encodeURIComponent(argument))
}
