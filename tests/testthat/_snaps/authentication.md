# authentication works

    Code
      authenticate(email = httr2::secret_decrypt(
        "PF4hPUkS3vBg88tAHPw4usSuACHd_pg92sBfKpJomc0KiwGpz32uap0BEb0", "FANTASY_KEY"),
      password = httr2::secret_decrypt("SjNB1d71cqrLJq6anSDPf_xWOrTzKH0RZXr20w",
        "FANTASY_KEY"))
    Message <cliMessage>
      i Sending login request for user [login]
      v Login successful, overwriting existing cookie...
      v Authentication successful!

