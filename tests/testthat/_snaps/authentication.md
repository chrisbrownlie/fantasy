# authentication works

    Code
      authenticate(email = httr2::secret_decrypt(
        "tPIPG8HBhvYfTY2j5dEEDfGdc2VcvclMi-TANtfDclBLYGiNYu4198rneTk", "FANTASY_KEY"),
      password = httr2::secret_decrypt("-SJx4AhW09y51joLmPSEWWAf5ezKkVX0EluSeQ",
        "FANTASY_KEY"))
    Message <cliMessage>
      i Sending login request for user [login]
      v Login successful, overwriting existing cookie...
      v Authentication successful!

