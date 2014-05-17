class Bob
  def hey(msg)
    if msg === msg.upcase and msg =~ /[A-Z]/
      'Woah, chill out!'
    elsif msg[-1] === '?'
      'Sure.'
    elsif msg.gsub(/\s+/, '') === ''
      'Fine. Be that way!'
    else
      'Whatever.'
    end
  end
end
