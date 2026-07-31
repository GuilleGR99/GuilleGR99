import os
import requests


GITHUB_API = "https://api.github.com"

def get_public_repositories(username):
    url = f"{GITHUB_API}/users/{username}/repos"

    response = requests.get(
        url,
        params={
            "type": "owner",
            "per_page": 100,
        },
        timeout=10,
    )

    response.raise_for_status()

    return response.json()


def main():
    username = os.environ["GITHUB_USERNAME"]
    repositories = get_public_repositories(username)


    for repository in repositories:
        print(repository["name"])


if __name__ == "__main__":
    main()
