#!/usr/bin/env python3
import os
import re
import sys
import tarfile
import time
from html.parser import HTMLParser
from urllib.error import HTTPError
from urllib.parse import urljoin, urlparse
from urllib.request import Request, urlopen

INDEX_URL = "https://katagoarchive.org/kata1/ratinggames/index.html"
OUTPUT_DIR = "downloads"
EXTRACT_DIR_NAME = "sgfs"
TARGET_YEAR = "2025"
USER_AGENT = "Mozilla/5.0"
CHUNK_SIZE = 1024 * 1024
NETWORK_TIMEOUT_SECONDS = 30
PROGRESS_EVERY_BYTES = 10 * 1024 * 1024
RETRIES = 6
RETRY_DELAY_SECONDS = 5
REQUEST_DELAY_SECONDS = 3


class LinkParser(HTMLParser):
    def __init__(self):
        super().__init__()
        self.links = []

    def handle_starttag(self, tag, attrs):
        if tag.lower() != "a":
            return
        for key, value in attrs:
            if key.lower() == "href" and value:
                self.links.append(value)


def fetch_text(url):
    request = Request(url, headers={"User-Agent": USER_AGENT})
    with urlopen(request, timeout=NETWORK_TIMEOUT_SECONDS) as response:
        charset = response.headers.get_content_charset() or "utf-8"
        return response.read().decode(charset, errors="replace")


def get_file_urls(index_url, year):
    html = fetch_text(index_url)
    parser = LinkParser()
    parser.feed(html)

    urls = []
    seen = set()
    pattern = re.compile(r"(?:^|/)(%s-\d\d-\d\drating\.tar\.bz2)$" % re.escape(year))

    for href in parser.links:
        absolute_url = urljoin(index_url, href)
        path = urlparse(absolute_url).path
        if not pattern.search(path):
            continue
        if absolute_url in seen:
            continue
        seen.add(absolute_url)
        urls.append(absolute_url)

    urls.sort()
    return urls


def archive_stem(filename):
    suffix = ".tar.bz2"
    if filename.endswith(suffix):
        return filename[:-len(suffix)]
    return filename


def unpack_file(archive_path, extract_dir):
    filename = os.path.basename(archive_path)
    marker_path = os.path.join(extract_dir, f".{archive_stem(filename)}.done")

    if os.path.exists(marker_path):
        print(f"Already unpacked: {filename}")
        return

    print(f"Unpacking: {filename}")
    os.makedirs(extract_dir, exist_ok=True)
    with tarfile.open(archive_path, "r:bz2") as archive:
        archive.extractall(extract_dir)

    with open(marker_path, "w", encoding="utf-8") as marker_file:
        marker_file.write("ok\n")

    print(f"Unpacked into {extract_dir}: {filename}")


def download_file(url, output_dir):
    filename = os.path.basename(urlparse(url).path)
    destination = os.path.join(output_dir, filename)
    temp_destination = destination + ".part"

    if os.path.exists(destination):
        print(f"Already downloaded: {filename}")
        return destination, False

    for attempt in range(1, RETRIES + 1):
        try:
            print(f"Downloading: {filename} (attempt {attempt}/{RETRIES})")
            request = Request(url, headers={"User-Agent": USER_AGENT})
            with urlopen(request, timeout=NETWORK_TIMEOUT_SECONDS) as response, open(temp_destination, "wb") as output_file:
                total_bytes = 0
                next_progress = PROGRESS_EVERY_BYTES
                while True:
                    chunk = response.read(CHUNK_SIZE)
                    if not chunk:
                        break
                    output_file.write(chunk)
                    total_bytes += len(chunk)
                    if total_bytes >= next_progress:
                        print(f"  received {total_bytes // (1024 * 1024)} MiB: {filename}")
                        next_progress += PROGRESS_EVERY_BYTES
            os.replace(temp_destination, destination)
            print(f"Downloaded: {filename}")
            return destination, True
        except HTTPError as exc:
            if os.path.exists(temp_destination):
                os.remove(temp_destination)
            wait_time = RETRY_DELAY_SECONDS * attempt
            if exc.code == 429:
                wait_time = max(wait_time, 30 * attempt)
            print(f"Attempt {attempt}/{RETRIES} failed for {filename}: {exc}")
            if attempt < RETRIES:
                print(f"Waiting {wait_time} seconds before retrying...")
                time.sleep(wait_time)
            else:
                raise
        except Exception as exc:
            if os.path.exists(temp_destination):
                os.remove(temp_destination)
            wait_time = RETRY_DELAY_SECONDS * attempt
            print(f"Attempt {attempt}/{RETRIES} failed for {filename}: {exc}")
            if attempt < RETRIES:
                print(f"Waiting {wait_time} seconds before retrying...")
                time.sleep(wait_time)
            else:
                raise


def main():
    output_dir = sys.argv[1] if len(sys.argv) > 1 else OUTPUT_DIR
    extract_dir = os.path.join(os.path.dirname(os.path.abspath(output_dir)), EXTRACT_DIR_NAME)
    os.makedirs(output_dir, exist_ok=True)
    os.makedirs(extract_dir, exist_ok=True)

    urls = get_file_urls(INDEX_URL, TARGET_YEAR)
    if not urls:
        print("No matching files found.")
        return 1

    print(f"Found {len(urls)} files for {TARGET_YEAR}.")
    for index, url in enumerate(urls, 1):
        filename = os.path.basename(urlparse(url).path)
        print(f"[{index}/{len(urls)}] {filename}")
        archive_path, downloaded = download_file(url, output_dir)
        unpack_file(archive_path, extract_dir)
        if downloaded and index < len(urls):
            print(f"Waiting {REQUEST_DELAY_SECONDS} seconds before next download...")
            time.sleep(REQUEST_DELAY_SECONDS)

    print("Done.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
