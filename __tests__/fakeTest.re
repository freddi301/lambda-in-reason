open Jest;

open ExpectJs;

let () =
  describe("fake test", () =>
    test("nothing", () =>
      expect("expectedOutput") |> toEqual("expectedOutput")
    )
  );