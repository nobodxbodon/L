<?php
Class Cart
{
    private $prices = [];
    private $products = [];

    public function Cart(){
        $this->prices['butter'] = 1.00;
        $this->prices['milk']   = 3.00;
        $this->prices['eggs']   = 6.95;
    }

    public function add($product_name, $quantity){
        $this->products[$product_name] = $quantity;
    }

    public function getTotal($tax_rate){
        $total_price = 0.0;
        $tax_factor = 1.0 + $tax_rate;

        array_walk($this->products, function($quantity, $product_name) use($tax_factor, &$total_price) {
            $total_price += $quantity * $this->prices[$product_name] * $tax_factor;
        });

        return round($total_price, 2);

    }
}

$my_cart = new Cart();

$my_cart->add('butter', 1);
$my_cart->add('milk', 3);
$my_cart->add('eggs', 6);

print $my_cart->getTotal(0.05) . "\n";