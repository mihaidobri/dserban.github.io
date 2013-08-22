# wget -O /tmp/exfm.json http://ex.fm/api/v3/song/search/parazitii

require 'json'

def read_raw_json(file_handle)
  f = open(file_handle, 'r')
  raw_json = f.readlines.join
  f.close

  raw_json
end

def parse_raw_json(raw_json)
  parsed_json = JSON.parse(raw_json)
  parsed_json
end

parsed_json = parse_raw_json(read_raw_json('/tmp/exfm.json'))

urls = parsed_json['songs'].map { |song| song['url'] }

urls.each do |url|
  puts "curl -I #{url}"
end

# curl -IL -w "%{http_code} %{content_type} %{url_effective}\\n" http://www.hulkshare.com/ap-h1wcbkkcszy8.mp3

# Example of an valid URL

# 200 audio/mpeg http://cdn01.hulkshare.com/dev1/0/005/089/0005089527.fid/Parazitii_feat__Dan_Lazar_-_Toate-s_la_fel.mp3?key=faf680a346fe2a2429680d561f2a8f23&dl=1

# Example of an invalid URL

# 403 text/html http://s1.fisierulmeu.ro:32681/dwn_links/600876f7472fca49a68c638d6bb801fc/4f8f022f/hd2/41/41WXZE0JM49N/name/%5Bwww.fisierulmeu.ro%5D%20Parazitii%20-%20Nu%20ma%20schimbi.mp3

require 'curb'

# url = 'http://ekiga.net/ip/'
url = 'http://www.hulkshare.com/ap-h1wcbkkcszy8.mp3'
# url = ''
# url = ''
# url = ''

c = Curl::Easy.http_head(url) do |curl|
  curl.headers["User-Agent"] = "Mozilla/5.0"
  curl.follow_location = true
  curl.enable_cookies = true
end

puts "http_code:    #{c.response_code}"
puts "content_type: #{c.content_type}"
