const express = require("express");
const router = express.Router();
const { Favorite, LogRequest } = require("../models");

// Mendapatkan semua data favorit
router.get("/get_favorites", async (req, res) => {
  try {
    const favorites = await Favorite.findAll();
    res.json(favorites);
  } catch (error) {
    res.status(500).json({ message: "Error fetching favorites" });
  }
});

// Menambahkan favorit
router.post("/add_favorite", async (req, res) => {
  const { from, to } = req.body;
  try {
    const favorite = await Favorite.create({
      from_currency: from,
      to_currency: to,
    });
    res.json({ status: "success", favorite });
  } catch (error) {
    res.status(500).json({ message: "Error adding favorite" });
  }
});

// menghapus favorit
router.delete("/delete_favorite/:id", async (req, res) => {
  const { id } = req.params;
  try {
    const deleted = await Favorite.destroy({
      where: { id: id }
    });

    if (deleted) {
      res.json({ status: "success", message: "Favorite deleted" });
    } else {
      res.status(404).json({ message: "Favorite not found" });
    }
  } catch (error) {
    res.status(500).json({ message: "Error deleting favorite" });
  }
});


// Menambahkan favorit
router.post("/log-request", async (req, res) => {
  const { from, to } = req.body;

  try {
    const existingRequest = await LogRequest.findOne({
      where: { from_to_currency: `${from}/${to}` },
    });

    if (existingRequest) {
      await LogRequest.update(
        { request_count: existingRequest.request_count + 1 },
        { where: { from_to_currency: `${from}/${to}` } }
      );
      res.json({ status: "success", message: "Request updated" });
      return;
    }

    const favorite = await LogRequest.create({
      from_to_currency: `${from}/${to}`,
      request_count: 1,
    });
    res.json({ status: "success", favorite });
  } catch (error) {
    res.status(500).json({ message: "Error adding favorite" });
  }
});

// Mendapatkan semua data favorit
router.get("/frequent-request", async (req, res) => {
  try {
    // sort by highest request number, limit 5
    const LogRequests = await LogRequest.findAll({
      order: [["request_count", "DESC"]],
      limit: 5,
    });

    res.json(LogRequests);
  } catch (error) {
    res.status(500).json({ message: "Error fetching favorites" });
  }
});


module.exports = router;
