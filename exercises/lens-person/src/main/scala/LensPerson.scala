import java.time.LocalDate

object LensPerson {
  case class Person(_name: Name, _born: Born, _address: Address)

  case class Name(_foreNames: String /*Space separated*/ , _surName: String)

  // Value of java.time.LocalDate.toEpochDay
  type EpochDay = Long

  case class Born(_bornAt: Address, _bornOn: EpochDay)

  case class Address(_street: String, _houseNumber: Int,
    _place: String /*Village / city*/ , _country: String)

  // Valid values of Gregorian are those for which 'java.time.LocalDate.of'
  // returns a valid LocalDate.
  case class Gregorian(_year: Int, _month: Int, _dayOfMonth: Int)

  // Implement these.

  val bornStreet: Born => String = _._bornAt._street

  val setCurrentStreet: String => Person => Person = { street => person =>
    person.copy(_address = person._address.copy(_street = street))
  }

  val setBirthMonth: Int => Person => Person = { month => person =>
    def fixDate(epochDay: EpochDay): EpochDay = {
      val localDate = LocalDate.ofEpochDay(epochDay)
      LocalDate.of(localDate.getYear, month, localDate.getDayOfMonth).toEpochDay
    }

    person.copy(_born = person._born.copy(_bornOn = fixDate(person._born._bornOn)))
  }

  // Transform both birth and current street names.

  val renameStreets: (String => String) => Person => Person = { streetTransform => person =>
    def addressTransform(address: Address) = address.copy(_street = streetTransform(address._street))
    def bornTransform(born: Born) = born.copy(_bornAt = addressTransform(born._bornAt))
    person.copy(_address = addressTransform(person._address), _born = bornTransform(person._born))
  }
}
