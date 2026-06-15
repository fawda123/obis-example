def main():
    print("Hello from obis!")


if __name__ == "__main__":
    main()

from pyobis import occurrences

# Search for occurrences
query = occurrences.search(scientificname="Mola mola")
data = query.execute()
